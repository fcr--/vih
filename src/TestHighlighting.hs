import Peg.PegParser
import Peg.Peg
import qualified Data.Map as M


hola = parseGrammar $ unlines [
  "all = (comment / string / reserved (' ' / '\n' / !.) / Cons / float / plain / ' ' / '\n' )*;",

  "comment = \"--\" (!'\n'.)*; # FALTA MULTILINEA",

  "string = '\"' (!'\"'.)* '\"';",

  "reserved  = '|' / \"..\" / \"::\" / '@' /  '~' / \"->\" / \"<-\" /  words;",

  "words = \"case\" / \"class\" / \"data\" / \"default\" / \"deriving\" / \"do\"",
  "      / \"mdo\" / \"else\" / \"forall\" / \"foreign\" / \"hiding\" / \"if\"",
  "      / \"import\" / \"in\" / \"infix\" / \"infixl\" / \"infixr\"",
  "      / \"instance\" / \"let\" / \"module\" / \"newtype\" / \"of\"",
  "      / \"qualified\" / \"then\" / \"where\" / \"type\" ;",

  "Cons = alphamay (alphamin / alphamay / num / \"'\"  / '_' / '#') *;",

  "float = num+ ('.' num+)?;",

  "plain = (!(num/' '/'\n').) (!(' '/'\n').)*;",

  "alphamin = 'a'..'z';",
  "alphamay = 'A'..'Z';",
  "num = '0'..'9';"]

test = pegMatch ( hola M.! "all" )  "case 99 of True -> of" 
