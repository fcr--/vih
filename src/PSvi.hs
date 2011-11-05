module PSvi (psParse,
             PSObject(..),
             psNewState,
             psInterp)
    where

import Peg.Peg
import Char
import Control.Applicative
import qualified Data.Map as M

data PSObject = PSString String
              | PSCode [PSObject]
              | PSInt Int
              | PSName String
              | PSMap (M.Map PSObject PSObject)
              | PSInternalOp String (PSState -> IO (Either String PSState))

instance Show PSObject where
    show (PSString s)       = "PSString " ++ show s
    show (PSCode c)         = "PSCode " ++ show c
    show (PSInt i)          = "PSInt " ++ show i
    show (PSName n)         = "PSName " ++ show n
    show (PSMap m)          = "PSMap " ++ show m
    show (PSInternalOp n _) = "PSInternalOp " ++ n

instance Eq PSObject where
    PSString s == PSString t             = s == t
    PSCode c1 == PSCode c2               = c1 == c2
    PSInt i == PSInt j                   = i == j
    PSName n == PSName m                 = n == m
    PSMap m1 == PSMap m2                 = m1 == m2
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
    compare (PSInternalOp o1 _) (PSInternalOp o2 _)  = compare o1 o2
    compare (PSInternalOp _ _) _                     = LT

-- ps = blanks (atom blanks)*
grm_ps = PegAster PSCode (
    PegCat head [
        grm_atom,
        grm_blanks undefined])

-- string = '"' ('\\' . / !('\\' / '"') .)* '"';
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
grm_atom = PegAlt id [grm_string, grm_number, grm_code, grm_name, grm_operators]

-- number = '-'? ('0'..'9'+)
grm_number = PegCat (PSInt . read . concat) [
    PegOpt concat (
        PegEqToken id "-"),
    PegPlus concat (
        PegTerm id (isDigit . head))]

-- name = ('a'..'z'/'A'..'Z'/'_') ('a'..'z'/'A'..'Z'/'_'/'0'..'9')*
grm_name = PegCat (PSName . concat) [
    PegTerm id (\[c] -> isAlpha c || c=='_'),
    PegAster concat (
        PegTerm id (\[c] -> isAlphaNum c || c=='_'))]

-- operators = '[' / ']' / '+' / '-' / '*' / '<' '<' / '>' '>'
grm_operators = PegAlt PSName [
    PegTerm id (\[c] -> elem c "[]+-*"),
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
    line = (length $ filter ('\n'==) $ errInfo') + 1
    errInfo = drop (n - len) errInfo' ++ "\27[7m" ++ take 1 ttext ++ "\27[0m" ++ drop 1 ttext
    errInfo' = take n text
    ttext = take len (drop n text) ++ " "
    len = 15



------ POSTSCRIPT INTERPRETER ------

data PSState = PSState {
        globDict  :: M.Map PSObject PSObject,
        dictStack :: [M.Map PSObject PSObject],
        stack     :: [PSObject]}
    deriving Show



psNewState :: PSState
psNewState = PSState {
        globDict = M.fromList globals,
        dictStack = [],
        stack = []}
    where
    globals = map (\(k,v) -> (PSString k, PSInternalOp k v)) [
        ("pop", psOpPop)]



psInterp :: PSState -> PSObject -> IO (Either String PSState)

-- psInterp for string objects (push):
psInterp st obj@(PSString s) = return $ Right st {stack = obj : stack st}

-- psInterp for code objects:
psInterp st obj@(PSCode (o:os)) = do
        e <- psInterp st o
        case e of
            Left m -> return e
            Right st' -> psInterp st' (PSCode os)

-- psInterp for empty code objects:
psInterp st obj@(PSCode []) = return $ Right st

-- psInterp for int objects (push):
psInterp st obj@(PSInt i) = return $ Right st {stack = obj : stack st}

-- psInterp for name objects (lookup):
psInterp st (PSName n) = case find n (dictStack st ++ [globDict st]) of
        Just obj -> psInterp st obj
        Nothing -> return $ Left ("psInterp error: name " ++ n ++ " not found.")
    where
    find n (d:ds) = M.lookup (PSString n) d <|> find n ds
    find n [] = Nothing

-- psInterp for map objects (push):
psInterp st obj@(PSMap m) = return $ Right st {stack = obj : stack st}

-- psInterp for internal objects (execute):
psInterp st obj@(PSInternalOp name op) = op st



------ POSTSCRIPT STACK OPERATORS ------

psOpPop :: PSState -> IO (Either String PSState)
psOpPop st = return $ case stack st of
    (s:ss) -> Right st {stack = ss}
    [] -> Left "psInterp error: empty stack."

-- vi: et sw=4
