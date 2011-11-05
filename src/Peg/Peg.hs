{-# Language GADTs #-}

module Peg.Peg (
  PegGrammar(..),
  pegMatch,
  pegMatch'
  ) where

import Data.Maybe
import Control.Monad

data PegGrammar a where
  -- match against a certain token:
  PegEqToken :: (String -> a) -> String         -> PegGrammar a
  -- match if the condition returns true given the token:
  PegTerm    :: (String -> a) -> (String -> Bool) -> PegGrammar a
  -- match if all the children match in order:
  PegCat     :: ([b] -> a)    -> [PegGrammar b] -> PegGrammar a
  -- match if some of the children match (children are always tried in order)
  PegAlt     :: (b -> a)      -> [PegGrammar b] -> PegGrammar a
  -- match against the child any (max posible) number of times:
  PegAster   :: ([b] -> a)    -> (PegGrammar b) -> PegGrammar a
  -- match if the child matches at least 1 time:
  PegPlus    :: ([b] -> a)    -> (PegGrammar b) -> PegGrammar a
  -- always matches, at most one time:
  PegOpt     :: ([b] -> a)    -> (PegGrammar b) -> PegGrammar a
  -- matches if the child matches, not consuming any input:
  PegPosLA   :: (b -> a)      -> (PegGrammar b) -> PegGrammar a
  -- matches if the child doesn't match, not consuming any input:
  PegNegLA   :: a             -> (PegGrammar b) -> PegGrammar a
  -- always match not consuming any input, may be used for other stuff...
  PegMeta    :: a                               -> PegGrammar a


pegMatch :: PegGrammar a -> [String] -> (Maybe a, [String])

pegMatch grammar tokens = let (r,t,_) = pegMatch' grammar tokens 0 in (r,t)


pegMatch' :: PegGrammar a -> [String] -> Int -> (Maybe a, [String], Int)

pegMatch' (PegEqToken fn str) tokens n
  | null tokens         = (Nothing, tokens, n)
  | head tokens == str  = (Just (fn str), tail tokens, n+1)
  | otherwise           = (Nothing, tokens, n)


pegMatch' (PegTerm fn m) tokens n
  | null tokens      = (Nothing, tokens, n)
  | m $ head tokens  = (Just (fn $ head tokens), tail tokens, n+1)
  | otherwise        = (Nothing, tokens, n)


pegMatch' (PegCat fn []) tokens n = (Just $ fn [], tokens, n)

pegMatch' (PegCat fn children) tokens n
  | isJust res = (res, toks, n')
  | otherwise  = (res, tokens, n')
    where
    (res, toks, n') = applyFst (fmap fn) $ head $ dropWhile (isJust . fst3) xs ++ [last xs]
    xs = pegMatchGen True children tokens n


pegMatch' (PegAlt _ []) tokens n = (Nothing, tokens, n)
pegMatch' (PegAlt fn alts) tokens n
  | isJust res = (res, toks, n')
  | otherwise  = (res, tokens, n')
    where
    (res, toks, n') = applyFst (fmap (fn . last)) $ head $
			dropWhile (isNothing . fst3) xs ++ [last xs]
    xs = pegMatchGen False alts tokens n


pegMatch' (PegAster fn child) tokens n =
  (fmap fn $ fst3 $ last ((Just [],[],0) : takeWhile (isJust . fst3) xs), toks, n')
    where
    xs = pegMatchGen True (repeat child) tokens n
    (_, toks, n') = head $ dropWhile (isJust . fst3) xs


pegMatch' (PegPlus fn child) tokens n =
  (fmap fn $ fst3 $ last (head xs : takeWhile (isJust . fst3) xs), toks, n')
    where
    xs = pegMatchGen True (repeat child) tokens n
    (_, toks, n') = head $ dropWhile (isJust . fst3) xs


pegMatch' (PegOpt fn child) tokens n =
  applyFst (fmap fn . (\x -> if isJust x then x else Just [])) $ head xs
    where
    xs = pegMatchGen True [child] tokens n


pegMatch' (PegPosLA fn child) tokens n = (fmap (fn . head) $ fst3 $ head xs, tokens, n)
  where
  xs = pegMatchGen True [child] tokens n


pegMatch' (PegNegLA k child) tokens n = (if j then Nothing else Just k, tokens, n)
    where
    j = isJust $ fst3 $ head $ pegMatchGen True [child] tokens n

pegMatch' (PegMeta k) tokens n = (Just k, tokens, n)

-- pegMatchGen:
--
--   The cases PegCat, PegAlt, PegPlus, PegAster... those whose constructor
-- take a PegGrammar or [PegGrammar] as children, can be generalized with the
-- function pegMatchGen.
--   pegMatchGen takes four arguments, a Bool indicating that it should stop
-- parsing any children after one of them fails, a list of children (all
-- children have type PegGrammar a), a list of tokens, and the token count.
--   It returns a list of tuples (r, t, c) of the same length as the list of
-- children passed by parameter, where each tuple represents an iteration over
-- the parsing of that child.
--   r: is Nothing if it failed or (if doFail was enabled and one of the
--      previous children failed).
--      or is Maybe [a0, a1 .. as] where this list is the result of parsing the
--      children up until the current one.
--   t: is the list of tokens remaining after parsing the item.
--   c: for a_i, it's max_{0..i} (let (_,_,c') in c')
--
-- TODO: re-run the examples...
--
-- Examples:
--   *Main> pegMatchGen True [PegEqToken MyStr "a"] $ ["a", "b", "c"] 0
--   [(Just [MyStr "a"],["b","c"], 1)]
--
--   *Main> pegMatchGen True [PegEqToken MyStr "a", PegAlt MyAlt [PegEqToken MyStr "a", PegEqToken MyStr "b"]] $ ["a", "b", "c"] 0
--   [(Just [MyStr "a"],["b","c"]),(Just [MyAlt (MyStr "b")],["c"])]
--
--   *Main> take 5 $ pegMatchGen True (repeat $ PegEqToken MyStr "a") $ ["a", "a", "c"] 0
--   [(Just [MyStr "a"],["a","c"]),(Just [MyStr "a",MyStr "a"],["c"]),(Nothing,["c"]),(Nothing,["c"]),(Nothing,["c"])]
--
--   *Main> take 5 $ pegMatchGen True [PegEqToken MyStr "b", PegEqToken MyStr "a"] $ ["a", "b", "c"] 0
--   [(Nothing,["a","b","c"]),(Nothing,["b","c"])]
--
--   *Main> take 5 $ pegMatchGen False [PegEqToken MyStr "b", PegEqToken MyStr "a"] $ ["a", "b", "c"] 0
--   [(Nothing,["a","b","c"]),(Just [MyStr "a"],["b","c"])]
--
-- It is used by some functions as:
--  PegPlus takes the last Just, or Nothing if there's no last, using repeat.
--  PegAster takes the last Just, or returns Just [] if there's no last, with repeat.
--  PegCat takes the last item, with a finite list of children.
--  PegAlt takes the first Just, with a finite list of children. (doFail=False)

pegMatchGen :: Bool -> [PegGrammar a] -> [String] -> Int -> [(Maybe [a], [String], Int)]
pegMatchGen doFail (child:children) tokens n =
  map (\(r,t,n,c,s) -> (r,t,n)) $
  takeWhile (\(r,t,n,c,stop) -> not stop) $
  iterate op $
  (\(r, t, n')->(fmap (:[]) r, t, n', children, False)) $
  pegMatch' child tokens n
    where
    fmap2' = if doFail then liftM2 else fmap2NoFail
    fmap2NoFail f x = liftM2 f (Just [])
    op (rl, toks, n, [], s) = (Nothing, toks, n, [], True) -- stop
    op (resl, toks, n, c:cs, s) = let (res, t, n') = pegMatch' c toks n in
	(fmap2' (\rl r -> rl++[r]) resl res, t, max n n', cs, False)



applyFst f (x, y, z) = (f x, y, z)


fst3 (a, _, _) = a
snd3 (_, b, _) = b


instance Show (PegGrammar a) where
  show (PegEqToken _ tok) = show tok
  show (PegTerm _ m) = "(func)"
  show (PegCat _ children) = "(" ++ concat t ++ ")"
    where t = zipWith (++) ("":repeat " ") (map show children)
  show (PegAlt _ children) = "(" ++ concat t ++ ")"
    where t = zipWith (++) ("":repeat " | ") (map show children)
  show (PegAster _ child) = show child ++ "*"
  show (PegPlus _ child) = show child ++ "+"
  show (PegOpt _ child) = show child ++ "?"
  show (PegPosLA _ child) = show child ++ "&"
  show (PegNegLA _ child) = show child ++ "!"
  show (PegMeta _) = "(meta)"

