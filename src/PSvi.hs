{-# LANGUAGE RankNTypes #-}

module PSvi (psParse,
             PSObject(..),
             psNewState,
             psInterp,
             psExec)
    where

import Peg.Peg
import Data.Char
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Bits as B

data PSObject = PSString String
              | PSCode [PSObject]
              | PSInt Int
              | PSName String
              | PSMap (M.Map PSObject PSObject)
              | PSList [PSObject]
              | PSInternalOp String (PSState -> IO (Either String PSState))

instance Show PSObject where
    show (PSString s)       = "PSString " ++ show s
    show (PSCode c)         = "PSCode " ++ show c
    show (PSInt i)          = "PSInt " ++ show i
    show (PSName n)         = "PSName " ++ show n
    show (PSMap m)          = "PSMap " ++ show m
    show (PSList l)         = "PSList " ++ show l
    show (PSInternalOp n _) = "PSInternalOp " ++ n

instance Eq PSObject where
    PSString s == PSString t             = s == t
    PSCode c1 == PSCode c2               = c1 == c2
    PSInt i == PSInt j                   = i == j
    PSName n == PSName m                 = n == m
    PSMap m1 == PSMap m2                 = m1 == m2
    PSList l1 == PSList l2               = l1 == l2
    PSInternalOp o _ == PSInternalOp p _ = o == p
    _ == _                               = False

instance Ord PSObject where
    compare (PSString s1) (PSString s2)              = compare s1 s2
    compare (PSString _) _                           = LT
    compare (PSCode c1) (PSCode c2)                  = compare c1 c2
    compare (PSCode _) _                             = LT
    compare (PSInt i1) (PSInt i2)                    = compare i1 i2
    compare (PSInt _) _                              = LT
    compare (PSName n1) (PSName n2)                  = compare n1 n2
    compare (PSName _) _                             = LT
    compare (PSMap m1) (PSMap m2)                    = compare m1 m2
    compare (PSMap _) _                              = LT
    compare (PSList l1) (PSList l2)                  = compare l1 l2
    compare (PSList _) _                             = LT
    compare (PSInternalOp o1 _) (PSInternalOp o2 _)  = compare o1 o2
    compare (PSInternalOp _ _) _                     = LT

-- ps = blanks (atom blanks)*
grm_ps :: PegGrammar PSObject
grm_ps = PegAster PSCode (
    PegCat head [
        grm_atom,
        grm_blanks undefined])

-- string = '"' ('\\' . / !('\\' / '"') .)* '"';
grm_string :: PegGrammar PSObject
grm_string = PegCat (PSString . concat) [
    PegEqToken (const "") "\"",
    PegAster id (PegAlt id [
        PegCat last [PegEqToken head "\\", PegTerm (unquote . head) (const True)],
        PegTerm head (\s -> (s/="\\") && (s/="\""))]),
    PegEqToken (const "") "\""]
    where
    unquote :: Char -> Char
    unquote c = last (c : [ x' | (x,x')<-unquoteList, x==c ])
    unquoteList = [('n','\n'), ('r','\r'), ('t','\t')]

-- code = '{' ps '}'
grm_code :: PegGrammar PSObject
grm_code = PegCat (!!2) [
    PegEqToken undefined "{",
    grm_blanks undefined,
    grm_ps,
    PegEqToken undefined "}"]

-- blanks = (' ' / '\t' / '\r' / '\n' / '%' (!'\n' .)* ('\n' / !.))*
grm_blanks :: a -> PegGrammar a
grm_blanks k = PegAster (const k) (
    PegAlt undefined [
        PegTerm undefined (isSpace . head),
        PegCat undefined [
            PegEqToken undefined "%",
            PegAster undefined (PegCat undefined [
                PegNegLA undefined (PegEqToken undefined "\n"),
                PegTerm undefined (const True)]),
            PegAlt undefined [
                PegEqToken undefined "\n",
                PegNegLA undefined (PegTerm undefined (const True))]]])

-- atom = string / number / code / name / operators
grm_atom :: PegGrammar PSObject
grm_atom = PegAlt id [grm_string, grm_number, grm_code, grm_name, grm_operators]

-- number = '-'? ('0'..'9'+)
grm_number :: PegGrammar PSObject
grm_number = PegCat (PSInt . read . concat) [
    PegOpt concat (
        PegEqToken id "-"),
    PegPlus concat (
        PegTerm id (isDigit . head))]

-- name = ('a'..'z'/'A'..'Z'/'_') ('a'..'z'/'A'..'Z'/'_'/'0'..'9')*
grm_name :: PegGrammar PSObject
grm_name = PegCat (PSName . concat) [
    PegTerm id (\c -> all isAlpha c || c=="_"),
    PegAster concat (
        PegTerm id (\c -> all isAlphaNum c || c=="_"))]

-- operators = '[' / ']' / '+' / '-' / '*' / '<' '<' / '>' '>'
grm_operators :: PegGrammar PSObject
grm_operators = PegAlt PSName [
    PegTerm id (\c -> c `elem` ["[", "]", "+", "-", "*"]),
    PegCat concat $ replicate 2 $ PegEqToken id "<",
    PegCat concat $ replicate 2 $ PegEqToken id ">"]

-- left side means an error.
psParse :: String -> Either String PSObject
psParse text = case res of
        Nothing -> Left ("Syntax error in postscript near char " ++ show n ++
                         ", on line " ++ show line ++ ": «" ++ errInfo ++ "»")
        Just x -> Right x
    where
    (res, _, n) = pegMatch' gram (map (:[]) text) 0
    -- grammar:
    gram = PegCat head [grm_ps, PegNegLA undefined $ PegTerm id $ const True]
    -- err info:
    line = length (filter ('\n'==) errInfo') + 1
    errInfo = drop (n - len) errInfo' ++ "\27[7m" ++ take 1 ttext ++ "\27[0m" ++ drop 1 ttext
    errInfo' = take n text
    ttext = take len (drop n text) ++ " "
    len = 15



------ POSTSCRIPT INTERPRETER ------

data PSState = PSState {
        globDict  :: M.Map PSObject PSObject,
        dictStack :: [M.Map PSObject PSObject],
        stack     :: [PSObject],
        retState  :: PSRetState}
    deriving Show

data PSRetState = PSRetOK | PSRetBreak
    deriving Show



psNewState :: PSState
psNewState = PSState {
        globDict = M.fromList globals,
        dictStack = [],
        stack = [],
        retState = PSRetOK}
    where
    globals = map (\(k,v) -> (PSString k, PSInternalOp k v)) [
        -- stack:
        ("copy", psOpCopy), ("count", psOpCount), ("dup", psOpDup),
        ("exch", psOpExch), ("index", psOpIndex), ("pop", psOpPop),
        ("roll", psOpRoll),
        -- arithmetic, math, and relational:
        ("+", psOpAdd),   ("-", psOpSub),   ("*", psOpMul),
        ("mod", psOpMod), ("div", psOpDiv), ("neg", psOpNeg),
        ("and", psOpAnd), ("or", psOpOr),   ("xor", psOpXor),
        ("not", psOpNot),
        ("eq", psOpEq "eq" (==)),
        ("ne", psOpEq "ne" (/=)),
        ("ge", psOpOrd "ge" (>)),
        ("gt", psOpOrd "gt" (>=)),
        ("le", psOpOrd "le" (<)),
        ("lt", psOpOrd "lt" (<=)),
        -- control:
        ("if", psOpIf), ("ifelse", psOpIfelse), ("for", psOpFor)
        ]



psInterp :: PSState -> PSObject -> IO (Either String PSState)

-- psInterp for name objects (lookup):
psInterp st (PSName n) = case find n (dictStack st ++ [globDict st]) of
        Just obj -> psExec st obj
        Nothing -> return $ Left ("psInterp error: (lookup): name " ++ n ++ " not found")
    where
    find name (d:ds) = M.lookup (PSString name) d <|> find name ds
    find _ [] = Nothing

-- psInterp for internal objects (execute):
psInterp st (PSInternalOp _ op) = op st

-- psInterp for map, string, int, code, list, etc... (push):
psInterp st obj = return $ Right st {stack = obj : stack st}



psExec :: PSState -> PSObject -> IO (Either String PSState)

-- psExec for code objects:
psExec st (PSCode (o:os)) = do
        e <- psInterp st o
        case e of
            Left _ -> return e
            Right st' -> case retState st' of
                PSRetOK -> psExec st' (PSCode os)
                _ -> return e

-- psExec for empty code objects:
psExec st (PSCode []) = return $ Right st

-- psExec's default case (call psInterp):
psExec st obj = psInterp st obj



ensureNArgs :: String -> Int -> PSState -> IO (Either String PSState) -> IO (Either String PSState)
ensureNArgs name n st action
    | (length . take n . stack) st < n  = return $ Left ("psInterp error: " ++ name ++ msg)
    | otherwise = action
    where
    msg = if n > 1 then ": at least " ++ show n ++ " elements on the stack required"
                   else ": empty stack"

------ POSTSCRIPT STACK OPERATORS ------

psOpCopy :: PSState -> IO (Either String PSState)
psOpCopy st = ensureNArgs "copy" 1 st $ return $ case stack st of
    (PSInt n:ss) -> let sub = take n ss in if length sub < n
        then Left "psInterp error: copy: not enough elements on the stack"
        else Right st {stack = sub ++ ss}
    _ -> Left "psInterp error: copy: not an int on top of the stack"

psOpCount :: PSState -> IO (Either String PSState)
psOpCount st = return $ Right st {stack = (PSInt $ length ss) : ss}
    where ss = stack st

psOpDup :: PSState -> IO (Either String PSState)
psOpDup st = ensureNArgs "dup" 1 st $ return $
    let (s:ss) = stack st in Right st {stack = s:s:ss}

psOpExch :: PSState -> IO (Either String PSState)
psOpExch st = ensureNArgs "exch" 2 st $ return $
    let (x:y:ss) = stack st in Right st {stack = y:x:ss}

psOpIndex :: PSState -> IO (Either String PSState)
psOpIndex st = ensureNArgs "index" 1 st $ return $ case stack st of
    (PSInt n:ss) -> let sub = take (n+1) ss in if length sub < (n+1)
        then Left "psInterp error: index: not enough elements on the stack"
        else Right st {stack = last sub : ss}
    _ -> Left "psInterp error: index: not an int on top of the stack"

psOpPop :: PSState -> IO (Either String PSState)
psOpPop st = ensureNArgs "pop" 1 st $ return $ Right st {stack = tail $ stack st}

psOpRoll :: PSState -> IO (Either String PSState)
psOpRoll st = ensureNArgs "roll" 2 st $ return $ case stack st of
    (PSInt i:PSInt n:ss) -> let sub = take n ss in if length sub < n
        then Left "psInterp error: roll: not enough elements on the stack"
        else Right st {stack = take n (drop (i `mod` n) $ cycle sub) ++ drop n ss}
    _ -> Left "psInterp error: roll: top 2 elements of the stack must be int"

------ POSTSCRIPT ARITHMETIC, MATH AND RELATIONAL OPERATORS ------

psOpAdd :: PSState -> IO (Either String PSState)
psOpAdd st = ensureNArgs "+" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (i1 + i2) : ss}
    (PSCode c1:PSCode c2:ss) -> Right st {stack = PSCode (c1 ++ c2) : ss}
    (PSList l1:PSList l2:ss) -> Right st {stack = PSList (l1 ++ l2) : ss}
    (PSString s1:PSString s2:ss) -> Right st {stack = PSString (s1 ++ s2) : ss}
    _ -> Left "psInterp error: +: types not matching or non int/code/list/string"

psOpSub :: PSState -> IO (Either String PSState)
psOpSub st = ensureNArgs "-" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (i2 - i1) : ss}
    _ -> Left "psInterp error: -: top 2 elements of the stack must be int"

psOpMul :: PSState -> IO (Either String PSState)
psOpMul st = ensureNArgs "*" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (i1 * i2) : ss}
    _ -> Left "psInterp error: *: top 2 elements of the stack must be int"

psOpMod :: PSState -> IO (Either String PSState)
psOpMod st = ensureNArgs "mod" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) -> if i2 /= 0
        then Right st {stack = PSInt (mod i2 i1) : ss}
        else Left "psInterp error: mod: division by zero"
    _ -> Left "psInterp error: mod: top 2 elements of the stack must be int"

psOpDiv :: PSState -> IO (Either String PSState)
psOpDiv st = ensureNArgs "div" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) -> if i2 /= 0
        then Right st {stack = PSInt (div i2 i1) : ss}
        else Left "psInterp error: div: division by zero"
    _ -> Left "psInterp error: div: top 2 elements of the stack must be int"

psOpNeg :: PSState -> IO (Either String PSState)
psOpNeg st = ensureNArgs "neg" 1 st $ return $ case stack st of
    (PSInt i:ss) -> Right st {stack = PSInt (-i) : ss}
    _ -> Left "psInterp error: neg: not an int on top of the stack"

psOpAnd :: PSState -> IO (Either String PSState)
psOpAnd st = ensureNArgs "and" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (i1 B..&. i2) : ss}
    _ -> Left "psInterp error: and: top 2 elements of the stack must be int"

psOpOr :: PSState -> IO (Either String PSState)
psOpOr st = ensureNArgs "or" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (i1 B..|. i2) : ss}
    _ -> Left "psInterp error: or: top 2 elements of the stack must be int"

psOpXor :: PSState -> IO (Either String PSState)
psOpXor st = ensureNArgs "xor" 2 st $ return $ case stack st of
    (PSInt i1:PSInt i2:ss) ->   Right st {stack = PSInt (B.xor i1 i2) : ss}
    _ -> Left "psInterp error: xor: top 2 elements of the stack must be int"

psOpNot :: PSState -> IO (Either String PSState)
psOpNot st = ensureNArgs "not" 1 st $ return $ case stack st of
    (PSInt i:ss) -> Right st {stack = PSInt (if i==0 then 1 else 0) : ss}
    _ -> Left "psInterp error: not: not an int on top of the stack"

psOpEq :: String -> (PSObject -> PSObject -> Bool) -> PSState -> IO (Either String PSState)
psOpEq name comp st = ensureNArgs name 2 st $ return $ case stack st of
    (o1:o2:ss) -> Right st {stack = PSInt (if comp o2 o1 then 1 else 0) : ss}
    _ -> error "ensureNArgs failed"

psOpOrd :: String -> (forall a. Ord a => a -> a -> Bool) -> PSState -> IO (Either String PSState)
psOpOrd name comp st = ensureNArgs name 2 st $ return $ case stack st of
    (PSInt i1 : PSInt i2 : ss) ->   Right st {stack = PSInt (if comp i2 i1 then 1 else 0) : ss}
    (PSString s1:PSString s2:ss) -> Right st {stack = PSInt (if comp s2 s1 then 1 else 0) : ss}
    _ -> Left ("psInterp error: " ++ name ++ ": types not matching or non int/string")

------ POSTSCRIPT CONTROL OPERATORS ------

psOpIf :: PSState -> IO (Either String PSState)
psOpIf st = ensureNArgs "if" 2 st $ case stack st of
    (PSCode c:PSInt i:ss) -> if i /= 0
        then psExec st {stack = ss} (PSCode c)
        else return $ Right st {stack = ss}
    _ -> return $ Left "psInterp error: if: top two elements must have int and code type (code on top)"

psOpIfelse :: PSState -> IO (Either String PSState)
psOpIfelse st = ensureNArgs "ifelse" 3 st $ case stack st of
    (f@(PSCode _):t@(PSCode _):PSInt i:ss) -> psExec st {stack = ss} (if i/=0 then t else f)
    _ -> return $ Left "psInterp error: ifelse: top three elements must have int, code and code type (code on top)"

psOpFor :: PSState -> IO (Either String PSState)
psOpFor st = ensureNArgs "for" 3 st $ case stack st of
    (proc@(PSCode _):l@(PSInt lim):i@(PSInt inc):b@(PSInt beg):ss) -> if lim < beg
        then return $ Right st {stack = ss}
        else do
            e <- psExec st {stack = b:ss} proc
            case e of
                Left _ -> return e
                Right st' -> case retState st' of
                    PSRetBreak -> return $ Right st' {retState = PSRetOK}
                    PSRetOK -> psOpFor st' {stack = proc:l:i:PSInt (beg+inc):stack st'}
    _ ->return $ Left "psInterp error: for: top four elements must have int, int, int and code type (code on top)"


-- vi: et sw=4
