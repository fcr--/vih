module Peg.PegGrammar where

import Debug.Trace

import Char
import Peg.Peg

data PegAst = Node String [PegAst]
	    | Terminal String -- matched text

data Meta = Gram {metaGram :: PegGrammar Meta}
	  | Grams [PegGrammar Meta] -- only used temporarily
	  | CapturedText String -- only used temporarily
	  | NonTerm String -- only used via PegMeta
	  | Def String Meta -- only used via PegMeta
  deriving (Show)

-- asigs = asig+ !dot
asigs :: PegGrammar Meta -- tras parsear queda: Grams [PegMeta $ Def n g, ...]
asigs = PegCat head [PegAster (Grams . map metaGram) asig,
  PegNegLA undefined (PegTerm undefined (const True))]

-- asig = blanks name blanks "=" blanks alt blanks ";"
asig :: PegGrammar Meta
asig = PegCat (\(_:Gram (PegMeta (NonTerm n)):_:e:_:g:xs) -> Gram $ PegMeta $ Def n g) [
	blanks, --only asig starts with blank
	name,
	blanks,
	PegEqToken undefined "=",
	blanks,
	alt,
	blanks,
	PegEqToken undefined ";",
	blanks]

-- alt = cat blanks ("/" blanks cat blanks)*
alt :: PegGrammar Meta
alt = PegCat (\(Gram g:_:[Grams gs]) -> Gram $ PegAlt undefined (g:gs)) [
	cat,
	blanks,
	PegAster (Grams . map metaGram) $
	  PegCat (!!2) [
	    PegEqToken undefined "/",
	    blanks,
	    cat,
	    blanks]]

cat :: PegGrammar Meta
cat = PegCat (\(Gram g:_:[Grams gs]) -> Gram $ PegCat undefined (g:gs)) [
	la,
	blanks,
	PegAster (Grams . map metaGram) $
	  PegCat head [
	    la,
	    blanks]]

la :: PegGrammar Meta
la = PegAlt id [
	PegCat (!!2) [
	  PegEqToken undefined "!",
	  blanks,
	  PegCat (Gram . PegNegLA undefined . metaGram . head) [cuant]],
	PegCat (!!2) [
	  PegEqToken undefined "&",
	  blanks,
	  PegCat (Gram . PegPosLA undefined . metaGram . head) [cuant]],
	cuant]

cuant :: PegGrammar Meta
cuant = PegCat f [
    atom,
    blanks,
    PegOpt (\gs -> if null gs then Grams [] else head gs) $
      PegTerm CapturedText (\s -> (s=="?") || (s=="*") || (s=="+"))]
  where
  f (a:_:[Grams []]) = a
  f (a:_:[CapturedText s])
    | s == "?" = Gram $ PegOpt   undefined $ metaGram a
    | s == "*" = Gram $ PegAster undefined $ metaGram a
    | s == "+" = Gram $ PegPlus  undefined $ metaGram a

atom = PegAlt id [ name,
  PegCat (!!1) [PegEqToken undefined "(", alt, PegEqToken undefined ")"],
  PegCat f1 [
    PegEqToken undefined "\"",
    string,
    PegEqToken undefined "\"",
    PegOpt f2 $
      PegCat (!!6) [
	PegPosLA undefined $
	  PegCat undefined [
	    blanks,
	    PegEqToken undefined ".",
	    PegEqToken undefined "."],
	blanks,
	PegEqToken undefined ".",
	PegEqToken undefined ".",
	blanks,
	PegEqToken undefined "\"",
	string,
	PegEqToken undefined "\""]]]
  where
  f1, f2 :: [Meta] -> Meta
  f2 [g] = g
  f2 _ = Grams []
  -- case when optional is null:
  f1 (_:CapturedText s:_:[Grams []]) = Gram $ PegEqToken undefined s
  -- case with optional: (range)
  f1 (_:CapturedText s1:_:[CapturedText s2]) =
    Gram $ PegTerm undefined (\s -> (s1 <= s) && (s <= s2))

name :: PegGrammar Meta
name = PegPlus (Gram . PegMeta . NonTerm . concat . map (\(CapturedText s) -> s)) $
	PegTerm CapturedText (and . map isAlpha)

string :: PegGrammar Meta
string = PegAster (CapturedText . concat . map (\(CapturedText s) -> s)) $
  PegAlt id [
    PegCat last [
      PegEqToken undefined "\\",
      PegTerm (CapturedText . ('\\':)) (const True)],
    PegCat last [
      PegNegLA undefined $ PegEqToken undefined "\"",
      PegTerm CapturedText (const True)]]

blanks :: PegGrammar Meta
blanks = PegAster undefined $ PegTerm undefined (and . map isSpace)

-- return a bunch of definitions:
parse :: String -> [Meta]
parse text = case res of
    Nothing -> error ("Syntax error in grammar near line " ++ show line ++ 
			"(" ++ show n ++ "): «" ++ errInfo ++ "»")
    Just _ -> ms
  where
  (res, _, n) = pegMatch' asigs (map (:[]) text) 0
  -- De-Meta-ize:
  ms = map (\(PegMeta m) -> m) $ (\(Just (Grams ms)) -> ms) res
  line = (length $ filter ('\n'==) $ errInfo') + 1
  errInfo = drop (n - len) errInfo' ++ "\27[7m" ++ take 1 ttext ++ "\27[0m" ++ drop 1 ttext
  errInfo' = take n text
  ttext = take len (drop n text) ++ " "
  len = 15
