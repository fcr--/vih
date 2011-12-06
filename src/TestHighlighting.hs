module TestHighlighting (highlight) where
import Peg.PegParser
import Peg.Peg
import Graphics.Vty.Attributes
import Data.Maybe (fromJust)
import qualified Data.Map as M

-- TODO : Add Multi-line comments

haskell = parseGrammar $ unlines [
  "all = (comment / string / reserved (' ' / '\n' / !.) / Cons / float / plain / ' ' / '\n' )*;",

  "comment = \"--\" (!'\n'.)*; # FALTA MULTILINEA",

  "string = '\"' (!'\"'.)* '\"';",

  "reserved  = '=' / '|' / \"..\" / \"::\" / '@' /  '~' / \"->\" / \"<-\" /  words;",

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

-- type -> Color, TODO : colors
colors :: M.Map String Color
colors = M.fromList [ ("comment", blue), ("string",red), ("reserved",bright_green), ("float",bright_red), ("Cons",green),("plain",black)]

-- TEST <----
test :: Either String [(Char,Color)]
test = highlight "case 99 of True -> of"

-- assign colors to each character
highlight :: String -> Either String [(Char,Color)]
highlight input = case pegMatch ( haskell M.! "all" )  input  of
			Right xs -> Right (map f xs)
			Left s	-> Left s -- should not happen .. ever
		where
--			if we have a type, go through the map
			f (c,(_:ty:_))	=	(c, M.findWithDefault black ty colors)
--			otherwise default to black
			f (c,_)		=	(c,black)

