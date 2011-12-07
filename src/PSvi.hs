{-# LANGUAGE RankNTypes #-}

module PSvi (main,psParse,
             PSObject(..),
             psNewState,
             psInterp,
             psExec)
    where

import Terminal
import TerminalInterfaces
import Peg.Peg
import Data.Char
import Control.Applicative
import Control.Concurrent.STM
import System.IO
import System(getArgs)
import System.Directory(getAppUserDataDirectory)
import System.FilePath(joinPath)
import qualified Data.Map as M
import qualified Data.Bits as B
import qualified Text.Regex.PCRE.String as PCRE

data PSObject = PSString !String
              | PSInt !Int
              | PSName !Bool !String -- True if name is literal
              | PSMap (M.Map PSObject PSObject)
              | PSList [PSObject]
              | PSInternalOp !String !(PSState -> IO (Either String PSState))
              | PSMark

instance Show PSObject where
    show (PSString s)       = "PSString " ++ show s
    show (PSInt i)          = "PSInt " ++ show i
    show (PSName lit n)     = "PSName " ++ (if lit then "/" else "") ++ show n
    show (PSMap m)          = "PSMap " ++ show m
    show (PSList l)         = "PSList " ++ show l
    show (PSInternalOp n _) = "PSInternalOp " ++ n
    show (PSMark)           = "PSMark"

instance Eq PSObject where
    PSString s == PSString t             = s == t
    PSInt i == PSInt j                   = i == j
    PSName _ n == PSName _ m             = n == m
    PSMap m1 == PSMap m2                 = m1 == m2
    PSList l1 == PSList l2               = l1 == l2
    PSInternalOp o _ == PSInternalOp p _ = o == p
    PSMark == PSMark                     = True
    _ == _                               = False

instance Ord PSObject where
    compare (PSString s1) (PSString s2)              = compare s1 s2
    compare (PSString _) _                           = LT
    compare (PSInt i1) (PSInt i2)                    = compare i1 i2
    compare (PSInt _) _                              = LT
    compare (PSName _ n1) (PSName _ n2)              = compare n1 n2
    compare (PSName _ _) _                           = LT
    compare (PSMap m1) (PSMap m2)                    = compare m1 m2
    compare (PSMap _) _                              = LT
    compare (PSList l1) (PSList l2)                  = compare l1 l2
    compare (PSList _) _                             = LT
    compare (PSInternalOp o1 _) (PSInternalOp o2 _)  = compare o1 o2
    compare (PSInternalOp _ _) _                     = LT
    compare PSMark PSMark                            = EQ
    compare PSMark _                                 = LT

-- ps = blanks (atom blanks)*
grm_ps :: PegGrammar PSObject
grm_ps = PegAster PSList (
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
grm_atom = PegAlt id [grm_string, grm_number, grm_code, grm_litname, grm_name, grm_operators]

-- number = '-'? ('0'..'9'+)
grm_number :: PegGrammar PSObject
grm_number = PegCat (PSInt . read . concat) [
    PegOpt concat (
        PegEqToken id "-"),
    PegPlus concat (
        PegTerm id (isDigit . head))]

-- litname = '/' ('a'..'z'/'A'..'Z'/'_') ('a'..'z'/'A'..'Z'/'_'/'0'..'9')*
grm_litname :: PegGrammar PSObject
grm_litname = PegCat (PSName True . tail . concat) [
    PegEqToken id "/",
    PegTerm id (\c -> all isAlpha c || c=="_"),
    PegAster concat (
        PegTerm id (\c -> all isAlphaNum c || c=="_"))]

-- name = ('a'..'z'/'A'..'Z'/'_') ('a'..'z'/'A'..'Z'/'_'/'0'..'9')*
grm_name :: PegGrammar PSObject
grm_name = PegCat (PSName False . concat) [
    PegTerm id (\c -> all isAlpha c || c=="_"),
    PegAster concat (
        PegTerm id (\c -> all isAlphaNum c || c=="_"))]

-- operators = '[' / ']' / '+' / '-' / '*' / '<' '<' / '>' '>'
grm_operators :: PegGrammar PSObject
grm_operators = PegAlt (PSName False) [
    PegTerm id (\c -> c `elem` ["[", "]", "+", "-", "*"]),
    PegCat concat $ replicate 2 $ PegEqToken id "<",
    PegCat concat $ replicate 2 $ PegEqToken id ">"]

-- left side means an error.
psParse :: String -> Either String PSObject
psParse = pegMatch gram
    where gram = PegCat head [grm_ps, PegNegLA undefined $ PegTerm id $ const True]



------ POSTSCRIPT INTERPRETER ------

data PSState = PSState {
        globDict  :: M.Map PSObject PSObject,
        dictStack :: ![M.Map PSObject PSObject],
        stack     :: ![PSObject],
        retState  :: !PSRetState,
        wtm :: Maybe WTManager}

instance Show PSState where
    show st = "globDict=" ++ show (globDict st) ++
        ", dictStack=" ++ show (dictStack st) ++
        ", stack=" ++ show (stack st) ++
        ", retState=" ++ show (retState st)

data PSRetState = PSRetOK | PSRetBreak
    deriving (Show, Eq)



psNewState :: IO PSState
psNewState = newTVarIO bm >>= \v -> return $ PSState {
        globDict = M.fromList globals,
        dictStack = [],
        stack = [],
        retState = PSRetOK,
        wtm = Nothing}
    where
    globals = map (\(k,v) -> (PSName False k, PSInternalOp k v)) [
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
        ("if", psOpIf),         ("ifelse", psOpIfelse), ("for", psOpFor),
        ("forall", psOpForall), ("repeat", psOpRepeat), ("loop", psOpLoop),
        ("exit", psOpExit),     ("exec", psOpExec),     --("filter", psOpFilter),
        ("try", psOpTry),
        -- data:
        ("[", psOpMark),        ("]", psOpCreateList),  ("<<", psOpMark),
        (">>", psOpCreateDict), ("length", psOpLength), ("null", psOpNull),
        ("def", psOpDef),       ("get", psOpGet),       ("known", psOpKnown),
        ("load", psOpLoad),     ("put", psOpPut),
        ("begin", psOpBegin),   ("end", psOpEnd),
        ("head", psOpHead),     ("tail", psOpTail),
        ("regexp", psOpRegexp), ("regsub", psOpRegsub),
        ("currentdict", psOpCurrentdict),
        ("type", psOpType),
        -- terminal:
        ("initwtm", psOpInitwtm),   ("getbuffsize", psOpGetbuffsize),
        ("getline", psOpGetline),   ("setline", psOpSetline),
        ("getxpos", psOpGetxpos),   ("getypos", psOpGetypos),
        ("setxpos", psOpSetxpos),   ("setypos", psOpSetypos),
        ("getxsize", psOpGetxsize), ("getysize", psOpGetysize),
        ("winup", psOpWinup),       ("windown", psOpWindown),
        ("openfile", psOpOpenfile), ("writefile", psOpWritefile)
        ]



psInterp :: PSState -> PSObject -> IO (Either String PSState)

-- psInterp for name objects (lookup):
psInterp st name@(PSName False n) = case find (dictStack st ++ [globDict st]) of
        Just obj -> psExec st obj
        Nothing -> return $ Left ("psInterp error: (lookup): name " ++ n ++ " not found")
    where
    find (d:ds) = M.lookup name d <|> find ds
    find [] = Nothing

-- psInterp for internal objects (execute):
psInterp st (PSInternalOp _ op) = op st

-- psInterp for map, string, int, code, list, etc... (push):
psInterp st obj = return $ Right st {stack = obj : stack st}



psExec :: PSState -> PSObject -> IO (Either String PSState)

-- psExec for code (list) objects:
psExec st (PSList (o:os)) = do
        e <- psInterp st o
        case e of
            Left _ -> return e
            Right st' -> case retState st' of
                PSRetOK -> psExec st' (PSList os)
                _ -> return e

-- psExec for empty code objects:
psExec st (PSList []) = return $ Right st

-- psExec's default case (call psInterp):
psExec st obj = psInterp st obj


psExecFile :: PSState -> FilePath -> IO (Either String PSState)

psExecFile st filename = do
    text <- catch (readFile filename) (const "")
    case psParse text of
        Left msg -> error text
        Right obj -> psExec st obj


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
    (PSList c:PSInt i:ss) -> if i /= 0
        then psExec st {stack = ss} (PSList c)
        else return $ Right st {stack = ss}
    _ -> return $ Left "psInterp error: if: top two elements must have int and code type (code on top)"

psOpIfelse :: PSState -> IO (Either String PSState)
psOpIfelse st = ensureNArgs "ifelse" 3 st $ case stack st of
    (f@(PSList _):t@(PSList _):PSInt i:ss) -> psExec st {stack = ss} (if i/=0 then t else f)
    _ -> return $ Left "psInterp error: ifelse: top three elements must have int, code(list) and code(list) type (code(list) on top)"

psOpFor :: PSState -> IO (Either String PSState)
psOpFor st = ensureNArgs "for" 3 st $ case stack st of
    (proc@(PSList _):l@(PSInt lim):i@(PSInt inc):b@(PSInt beg):ss) -> if lim < beg
        then return $ Right st {stack = ss}
        else do
            e <- psExec st {stack = b:ss} proc
            case e of
                Left _ -> return e
                Right st' -> case retState st' of
                    PSRetBreak -> return $ Right st' {retState = PSRetOK}
                    PSRetOK -> psOpFor st' {stack = proc:l:i:PSInt (beg+inc):stack st'}
    _ ->return $ Left "psInterp error: for: top four elements must have int, int, int and code(list) type (code (list) on top)"

psOpForall :: PSState -> IO (Either String PSState)
psOpForall st = ensureNArgs "forall" 2 st $ case stack st of
    (proc@(PSList _):PSList list:ss) -> forall' proc [ [o] | o <- list ]
    (proc@(PSList _):PSString str:ss) -> forall' proc [ [PSString c] | c <- str]
    (proc@(PSList _):PSMap dict):ss) -> forall' proc [ [k, v] | (k,v) <- M.toList dict ]
    _ ->return $ Left "psInterp error: forall: top two elements must have dict/string/list and code(list) type (code(list) on top)"
    where
    forall :: PSState -> PSObject -> [[PSObject]] -> IO (Either String PSState)
    forall st' proc [] = return $ Right st'
    forall st' proc (objs:objss) = do
        e <- psExec st' {stack = objs ++ stack st'} proc
        case e of
            Left _ -> return e
            Right st'' -> case retState st'' of
                PSRetBreak -> return $ Right st'' {retState = PSRetOK}
                PSRetOK -> forall st'' proc objss
    forall' = forall st {stack = drop 2 $ stack st}

psOpRepeat :: PSState -> IO (Either String PSState)
psOpRepeat st = ensureNArgs "repeat" 2 st $ case stack st of
    (proc@(PSList _) : PSInt count : ss) -> if count < 0
        then return $ Right st {stack = ss}
        else do
            e <- psExec st {stack = ss} proc
            case e of
                Left _ -> return e
                Right st' -> case retState st' of
                    PSRetBreak -> return $ Right st' {retState = PSRetOK}
                    PSRetOK -> psOpRepeat st' {stack = proc:PSInt (count-1):stack st'}
    _ ->return $ Left "psInterp error: repeat: top four elements must have int and code type (code(list) on top)"

psOpLoop :: PSState -> IO (Either String PSState)
psOpLoop st = ensureNArgs "loop" 1 st $ case stack st of
    (proc@(PSList _) : ss) -> do
        e <- psExec st {stack = ss} proc
        case e of
            Left _ -> return e
            Right st' -> case retState st' of
                PSRetBreak -> return $ Right st' {retState = PSRetOK}
                PSRetOK -> psOpLoop st' {stack = proc:stack st'}
    _ ->return $ Left "psInterp error: loop: not a code (list) on top of the stack"

psOpExit :: PSState -> IO (Either String PSState)
psOpExit st = return $ Right (if retState st==PSRetOK then st {retState=PSRetBreak} else st)

psOpExec :: PSState -> IO (Either String PSState)
psOpExec st = ensureNArgs "exec" 1 st $ case stack st of
    (o:ss) -> psExec st {stack = ss} o
    _ -> return $ Left "psInterp error: exec: empty stack"

psOpTry :: PSState -> IO (Either String PSState)
psOpTry st = ensureNArgs "try" 1 st $ case stack st of
    (PSList o:ss) -> psExec st {stack = ss} o >>= \res -> case res of
        Right st' -> return $ Right st' {stack = PSString "" : stack st'}
        Left msg -> return $ Right st {stack = PSString msg : ss}
    _ -> return $ Left "psInterp error: try: not a code (list) on top of the stack"

------ POSTSCRIPT DATA OPERATORS ------

psOpMark :: PSState -> IO (Either String PSState)
psOpMark st = return $ Right st {stack = PSMark : stack st}

psOpCreateList :: PSState -> IO (Either String PSState)
psOpCreateList st
    | null after = return $ Left "psInterp error: ]: mark not found in the stack"
    | otherwise = return $ Right st {stack = PSList (reverse before) : tail after}
    where
    ss = stack st
    (before, after) = break (PSMark ==) ss

psOpCreateDict :: PSState -> IO (Either String PSState)
psOpCreateDict st
    | null after = return $ Left "psInterp error: >>: mark not found in the stack"
    | length before `mod` 2 == 1 = return $ Left "psInterp error: >>: odd number of elements"
    | otherwise = return $ Right st {stack = map : tail after}
    where
    ss = stack st
    (before, after) = break (PSMark ==) ss
    pairs (v:k:ss) = (k, v) : pairs ss
    pairs [] = []
    map = PSMap $ M.fromList $ reverse $ pairs before

psOpLength :: PSState -> IO (Either String PSState)
psOpLength st = ensureNArgs "length" 1 st $ case stack st of
    (PSString s : ss) -> return $ Right st {stack = PSInt (length s) : ss}
    (PSMap m : ss) -> return $ Right st {stack = (PSInt . M.size) m : ss}
    (PSList l : ss) -> return $ Right st {stack = PSInt (length l) : ss}
    _ ->return $ Left "psInterp error: length: not a string/dict/list on top of the stack"

psOpNull :: PSState -> IO (Either String PSState)
psOpNull st = ensureNArgs "null" 1 st $ case stack st of
    (PSString s : ss) -> return $ Right st {stack = PSInt (if null s then 1 else 0) : ss}
    (PSMap m : ss) -> return $ Right st {stack = PSInt (if M.size m == 0 then 1 else 0) : ss}
    (PSList l : ss) -> return $ Right st {stack = PSInt (if null l then 1 else 0) : ss}
    _ ->return $ Left "psInterp error: length: not a string/dict/list on top of the stack"

psOpDef :: PSState -> IO (Either String PSState)
psOpDef st = ensureNArgs "def" 2 st $ case stack st of
    (v:k:ss) -> case dictStack st of
        [] -> return $ Right st {globDict = M.insert k v $ globDict st, stack = ss}
        (d:ds) -> return $ Right st {dictStack = M.insert k v d : ds, stack = ss}

psOpGet :: PSState -> IO (Either String PSState)
psOpGet st = ensureNArgs "get" 2 st $ case stack st of
    (key : PSMap dict : ss) -> return $ if key `M.member` dict
        then Right st {stack = (dict M.! key) : ss}
        else Left "psInterp error: get: key not in dictionary"
    (PSInt idx : PSString str : ss) -> return $ if idx >= 0 && idx < length str
        then Right st {stack = PSString (str !! idx) : ss}
        else Left "psInterp error: get: index out of bounds"
    (PSInt idx : PSList list : ss) -> return $ if idx >= 0 && idx < length list
        then Right st {stack = (list !! idx) : ss}
        else Left "psInterp error: get: index out of bounds"
    _ -> return $ Left "psInterp error: get: type error (check documentation PSvi.txt)"

psOpKnown :: PSState -> IO (Either String PSState)
psOpKnown st = ensureNArgs "known" 2 st $ case stack st of
    (key : PSMap dict : ss) -> return $ Right st {stack = PSInt (if key `M.member` dict then 1 else 0) : ss}
    (obj : PSList list : ss) -> return $ Right st {stack = PSInt (if obj `elem` list then 1 else 0) : ss}
    _ -> return $ Left "psInterp error: known: type error (check documentation)"

psOpLoad :: PSState -> IO (Either String PSState)
psOpLoad st = ensureNArgs "load" 1 st $ case find (dictStack st ++ [globDict st]) of
        Just obj -> return $ Right st {stack = obj : ss}
        Nothing -> return $ Left "psInterp error: load: key not found"
    where
    (key : ss) = stack st
    find (d:ds) = M.lookup key d <|> find ds
    find [] = Nothing

psOpPut :: PSState -> IO (Either String PSState)
psOpPut st = ensureNArgs "put" 3 st $ case stack st of
    (k:v:PSMap dict:ss) -> return $ Right st {stack = PSMap (M.insert k v dict) : ss}
    _ -> return $ Left "psInterp error: put: third element on the stack from top must be a dictionary"

psOpBegin :: PSState -> IO (Either String PSState)
psOpBegin st = return $ Right st { dictStack = M.empty : dictStack st }

psOpEnd :: PSState -> IO (Either String PSState)
psOpEnd st = return $ case dictStack st of
    [] -> Left "psInterp error: end: empty dictionary stack"
    (_:ds) -> Right st { dictStack = ds }

psOpHead :: PSState -> IO (Either String PSState)
psOpHead st = ensureNArgs "head" 1 st $ case stack st of
    (PSString s : ss) -> return $ case s of
        (c:_) -> Right st {stack = PSString [c] : ss}
        _ -> Left "psInterp error: head: null string"
    (PSList s : ss) -> return $ case s of
        (i:_) -> Right st {stack = i : ss}
        _ -> Left "psInterp error: head: null list"
    _ ->return $ Left "psInterp error: head: not a string/list on top of the stack"

psOpTail :: PSState -> IO (Either String PSState)
psOpTail st = ensureNArgs "tail" 1 st $ case stack st of
    (PSString s : ss) -> return $ case s of
        (_:cs) -> Right st {stack = PSString cs : ss}
        _ -> Left "psInterp error: tail: null string"
    (PSList s : ss) -> return $ case s of
        (_:is) -> Right st {stack = PSList is : ss}
        _ -> Left "psInterp error: tail: null list"
    _ ->return $ Left "psInterp error: tail: not a string/list on top of the stack"

psOpRegexp :: PSState -> IO (Either String PSState)
psOpRegexp st = ensureNArgs "regexp" 2 st $ case stack st of
    (PSString subj:PSString re:ss) -> do
        res <- PCRE.compile PCRE.compBlank PCRE.execBlank re
        case res of
            Left _ -> return $ Right $ st {stack = PSInt 0 : stack st}
            Right regex -> do
                res <- PCRE.regexec regex subj
                return $ case res of
                    Left _ -> Right $ st {stack = PSInt 0 : ss}
                    Right result -> Right $ st {stack = PSInt 1 : ss}
    _ -> return $ Left "psInterp error: regexp: types not string, string on top of the stack"

psOpRegsub :: PSState -> IO (Either String PSState)
psOpRegsub st = ensureNArgs "regsub" 3 st $ case stack st of
    (PSString replstr:PSString subj:PSString re:ss) -> do
        res <- PCRE.compile PCRE.compBlank PCRE.execBlank re
        case res of
            Left _ -> return $ Right st {stack = PSString subj : stack st}
            Right regex -> do
                res <- PCRE.regexec regex subj
                return $ case res of
                    Left _ -> Right st {stack = PSString subj : ss}
                    Right result -> Right st {stack = PSString (repl replstr subj result) : ss}
    _ -> return $ Left "psInterp error: regsub: types not string, string on top of the stack"
    where
    repl :: String -> String -> Maybe (String, String, String, [String]) -> String
    repl repl subj r = case r of Nothing -> subj; Just res@(p,_,po,_) -> p ++ repl' repl res ++ po
    repl' :: String -> (String, String, String, [String]) -> String
    repl' (b:c:cs) res@(pre, matched, post, groups)
        | b == '\\' && '1' <= c && c <= '9' = ((groups ++ repeat "") !! (fromEnum c - 48)) ++ repl' cs res
        | b == '\\' = c : repl' cs res
        | b == '&' = matched ++ repl' (c:cs) res
        | otherwise = b : repl' (c:cs) res
    repl' (c:cs) res@(pre, matched, post, groups)
        | c == '&' = matched ++ repl' cs res
        | otherwise = c : repl' cs res
    repl' "" _ = ""

psOpCurrentdict :: PSState -> IO (Either String PSState)
psOpCurrentdict st = return $ Right st {stack = PSMap dict : stack st}
    where dict = (if null (dictStack st) then head . dictStack else globDict) st

psOpType :: PSState -> IO (Either String PSState)
psOpType st = ensureNArgs "type" 1 st $ case stack st of
    (PSString _ : ss) -> return $ Right st {stack = PSString "string" : ss}
    (PSInt _ : ss) -> return $ Right st {stack = PSString "int" : ss}
    (PSName _ _: ss) -> return $ Right st {stack = PSString "name" : ss}
    (PSMap _ : ss) -> return $ Right st {stack = PSString "map" : ss}
    (PSList _ : ss) -> return $ Right st {stack = PSString "list" : ss}
    (PSInternalOp _ _: ss) -> return $ Right st {stack = PSString "internal" : ss}
    (PSMark _ : ss) -> return $ Right st {stack = PSString "mark" : ss}

------ BUFFER MANAGER WRAPPER ------

psOpInitwtm :: PSState -> IO (Either String PSState)
psOpInitwtm st = case wtm st of
    Nothing -> initWTM >> Return $ Right st
    Just wtm -> return $ Right st

psOpGetbuffsize :: PSState -> IO (Either String PSState)
psOpGetbuffsize st = return $ Right st {stack = PSInt (getBufSize (wtm st)) : stack st}

psOpGetline :: PSState -> IO (Either String PSState)
psOpGetline st = ensureNArgs "getline" 1 st $ case stack st of
    (PSInt line : ss) -> return $ Right st {stack = PSString (getLine (wtm st) line) : ss}
    _ ->return $ Left "psInterp error: getline: not an int on top of the stack"

psOpSetline :: PSState -> IO (Either String PSState)
psOpSetline st = ensureNArgs "setline" 2 st $ case stack st of
    (PSString txt : PSInt num : ss) -> do
        wtm' <- setline (wtm st) num txt
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: setline: not an int, string on top of the stack (string topmost)"

psOpGetxpos :: PSState -> IO (Either String PSState)
psOpGetxpos st = return $ Right st {stack = PSInt (getXpos (wtm st)) : stack st}

psOpGetypos :: PSState -> IO (Either String PSState)
psOpGetypos st = return $ Right st {stack = PSInt (getYpos (wtm st)) : stack st}

psOpSetxpos :: PSState -> IO (Either String PSState)
psOpSetxpos st = ensureNArgs "setxpos" 1 st $ case stack st of
    (PSInt pos : ss) -> do
        wtm' <- setXpos (wtm st) pos
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: setxpos: not an int on top of the stack"

psOpSetypos :: PSState -> IO (Either String PSState)
psOpSetypos st = ensureNArgs "setypos" 1 st $ case stack st of
    (PSInt pos : ss) -> do
        wtm' <- setYpos (wtm st) pos
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: setypos: not an int on top of the stack"

psOpGetxsize :: PSState -> IO (Either String PSState)
psOpGetxsize st = return $ Right st {stack = PSInt (getXsize (wtm st)) : stack st}

psOpGetysize :: PSState -> IO (Either String PSState)
psOpGetysize st = return $ Right st {stack = PSInt (getYsize (wtm st)) : stack st}

psOpWinup :: PSState -> IO (Either String PSState)
psOpWinup st = winUp (wtm st) >>= \w -> return $ Right st {wtm = w}

psOpWindown :: PSState -> IO (Either String PSState)
psOpWindown st = winDown (wtm st) >>= \w -> return $ Right st {wtm = w}

-- 'o' True, 'O' False.
psOpOpenline :: PSState -> IO (Either String PSState)
psOpOpenline st = ensureNArgs "openline" 1 st $ case stack st of
    (PSInt after : ss) -> do
        wtm' <- openLine (after /= 0) (wtm st)
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: openline: not an int on top of the stack"

psOpOpenfile :: PSState -> IO (Either String PSState)
psOpOpenfile st = ensureNArgs "openfile" 1 st $ case stack st of
    (PSString filename : ss) -> do
        wtm' <- openFile (wtm st) filename
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: openfile: not a string on top of the stack"

psOpWritefile :: PSState -> IO (Either String PSState)
psOpWritefile st = ensureNArgs "writefile" 1 st $ case stack st of
    (PSList [PSString filename] : ss) -> do
        wtm' <- writeFile (wtm st) (Just filename)
        return $ Right st {stack = ss, wtm = wtm'}
    (PSList [] : ss) -> do
        wtm' <- writeFile (wtm st) Nothing
        return $ Right st {stack = ss, wtm = wtm'}
    _ ->return $ Left "psInterp error: openfile: an empty list or a list with the filename must be on top of the stack"

------ MAIN LOOP ------

main = do
    s <- psNewState
    args <- getArgs
    let st = s {globDict = M.insert (PSName False "args") (PSList $ map PSString args) $ globDict s}

    -- on posix (unix/linux/mac): $HOME/.appName
    -- on windows: C:/Documents And Settings/user/Application Data/appName (or something like that)
    home <- getAppUserDataDirectory

    psExecFile st "init.ps"
    psExecFile st "/etc/init.ps"
    psExecFile st [joinPath home "init.ps"]
    psExecFile st "run.ps"
    psExecFile st "/etc/run.ps"
    terminal st

terminal :: PSState -> IO a
terminal s = do
        putStr ((show $ length $ stack s) ++ "> ")
        hFlush stdout
        line <- getLine
        case psParse line of
            Left m -> putStrLn m >> terminal s
            Right c -> psExec s c >>= \r -> case r of
                Left m -> putStrLn m >> terminal s
                Right s' -> print s' >> terminal s'

-- vi: et sw=4
