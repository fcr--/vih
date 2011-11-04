module PSvi where

import Peg.Peg
import Char

data PSObject = PSString String
              | PSCode [PSObject]
              | PSInt Int
              | PSName String
  deriving Show

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

-- vi: et sw=4
