module Peg.PegParser where

import Debug.Trace

import Char
import Peg.Peg
import qualified Data.Map as M

data PegAst = Node String [PegAst]
	    | Terminal String -- matched text

type Defs = M.Map String (PegGrammar [(Char,[String])])

-- asigs = asig+ !dot
asigs :: Defs -> PegGrammar Defs
asigs defs = PegCat head [PegAster M.fromList (asig defs),
  PegNegLA undefined (PegTerm undefined (const True))]

-- asig = blanks name blanks "=" blanks alt blanks ";"
asig :: Defs -> PegGrammar (String, PegGrammar [(Char,[String])])
asig defs = PegCat (\(_:Left n:_:e:_:Right g:xs) -> (n, mapName n g)) [
        blanks, --only asig starts with blank
        PegPlus (Left . concat) $ PegTerm id (and . map isAlpha),
        blanks,
        PegEqToken undefined "=",
        blanks,
        PegAlt Right [alt defs],
        blanks,
        PegEqToken undefined ";",
        blanks]
    where
    mapName :: String -> PegGrammar [(Char, [String])] -> PegGrammar [(Char, [String])]
    mapName n g = PegAlt (map (\(c, ns) -> (c, n:ns))) [g]

-- alt = cat blanks ("/" blanks cat blanks)*
alt :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
alt defs = PegCat (\([g]:_:[gs]) -> PegAlt id (g:gs)) [
	PegAlt (:[]) [cat defs],
	blanks,
	PegAster id $
	  PegCat (!!2) [
	    PegEqToken undefined "/",
	    blanks,
	    cat defs,
	    blanks]]

-- cat = la blanks (la blanks)*;
cat :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
cat defs = PegCat (\([g]:_:[gs]) -> PegCat concat (g:gs)) [
	PegAlt (:[]) [la defs],
	blanks,
	PegAster id $
	  PegCat head [
	    la defs,
	    blanks]]

la :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
la defs = PegAlt id [
	PegCat (!!2) [
	  PegEqToken undefined "!",
	  blanks,
	  PegCat (PegNegLA [] . head) [cuant defs]],
	PegCat (!!2) [
	  PegEqToken undefined "&",
	  blanks,
	  PegCat (PegPosLA (const []) . head) [cuant defs]],
	cuant defs]

cuant :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
cuant defs = PegCat f [
    PegAlt Left [atom defs],
    blanks,
    PegOpt Right $ PegTerm head (\[c] -> elem c "?*+")]
  where
  f (Left a:_:[Right s])
    | s == "?" = PegOpt   concat a
    | s == "*" = PegAster concat a
    | s == "+" = PegPlus  concat a
    | otherwise = a

atom :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
atom defs = PegAlt id [ name defs,
  PegCat (!!1) [PegEqToken undefined "(", alt defs, PegEqToken undefined ")"],
  PegCat (f1 . head . tail) [
    PegEqToken undefined "\"",
    string,
    PegEqToken undefined "\""],
  PegCat range [
    PegEqToken undefined "'",
    PegCat id [char],
    PegEqToken undefined "'",
    PegOpt id $
      PegCat (!!5) [
	blanks,
	PegEqToken undefined ".",
	PegEqToken undefined ".",
	blanks,
	PegEqToken undefined "'",
	char,
	PegEqToken undefined "'"]]]
  where
  f1 :: String -> PegGrammar [(Char, [String])]
  f1 s = PegCat id $ map (\c -> PegEqToken (\[c]->(c,[])) [c]) s
  range (_:[f]:_:[t]:_) = PegTerm (\[c]->[(c,[])]) (\[c] -> f<=c && c<=t)
  range (_:[f]:_:[]:_) = PegEqToken (const [(f, [])]) [f]

name :: Defs -> PegGrammar (PegGrammar [(Char, [String])])
name defs = PegPlus (defs M.!) $ PegTerm head (and . map isAlpha)

char :: PegGrammar Char
char = PegAlt id [
    PegCat last [
      PegEqToken undefined "\\",
      PegTerm (\[c] -> M.findWithDefault c c escaped) (const True)],
    PegCat last [
      PegNegLA undefined $ PegEqToken undefined "\'",
      PegTerm head (const True)]]
  where
  escaped = M.fromList [('n','\n'),('r','\r'),('t','\t')]

string :: PegGrammar String
string = PegAster id $
  PegAlt id [
    PegCat last [
      PegEqToken undefined "\\",
      PegTerm (\[c] -> M.findWithDefault c c escaped) (const True)],
    PegCat last [
      PegNegLA undefined $ PegEqToken undefined "\"",
      PegTerm head (const True)]]
  where
  escaped = M.fromList [('n','\n'),('r','\r'),('t','\t')]

blanks :: PegGrammar a
blanks = PegAster undefined $ PegTerm undefined (and . map isSpace)

-- return a bunch of definitions:
parseGrammar :: String -> Defs
parseGrammar text = case res of
    Left msg -> error msg
    Right _ -> defs
  where
  res = pegMatch (asigs defs) text
  defs = case res of
    Right d -> d
    _ -> M.empty
