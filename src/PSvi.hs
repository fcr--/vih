module PSvi where

import Peg.Peg
import Char

data PSObject = PSString String
  deriving Show

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

-- code = '{' blanks (atom blanks)* '}'

-- blanks = (' ' / '\t' / '\r' / '\n' / '%' (!'\n' .)* ('\n' / !.))*
grm_blank :: a -> PegGrammar a
grm_blank k = PegAster (const k) (
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
