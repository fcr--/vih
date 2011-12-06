module TestHighlighting (highlight) where
import Peg.PegParser hiding (char)
import Peg.Peg
import Graphics.Vty
import Data.Maybe (fromJust)
import qualified System.IO as S (readFile)
import qualified Data.Map as M

-- TODO : Testing


haskell = parseGrammar $ unlines [
  "all = (comment / string / reserved (' ' / '\n' / !.) / Cons / float / plain / . )*;",

  "comment = \"--\" (!'\n'.)* / multicomment  ; ",

  "multicomment = \"{-\" (\"--\" / multicomment / !(\"-}\").)* \"-}\" ? ;",

  "string =  '\"' (!'\"'.)* '\"';",

  "reserved  = '=' / '|' / \"..\" / \"::\" / '@' /  '~' / \"->\" / \"<-\" /  words;",

  "words = \"case\" / \"class\" / \"data\" / \"default\" / \"deriving\" / \"do\"",
  "      / \"mdo\" / \"else\" / \"forall\" / \"foreign\" / \"hiding\" / \"if\"",
  "      / \"import\" / \"in\" / \"infix\" / \"infixl\" / \"infixr\"",
  "      / \"instance\" / \"let\" / \"module\" / \"newtype\" / \"of\"",
  "      / \"qualified\" / \"then\" / \"where\" / \"type\" ;",

  "Cons = alphamay (alphamin / alphamay / num / \"'\"  / '_' / '#' / '.') *;",

  "float = num+ ('.' num+)?;",

  "plain = (alphamin / alphamay / num)+;",

  "alphamin = 'a'..'z';",
  "alphamay = 'A'..'Z';",
  "num = '0'..'9';"]

-- atributos de fondo.
def = def_attr

color = with_fore_color def

bold' = flip with_style $ bold 

-- type -> Color, TODO : colors
colors :: M.Map String (Char -> Image)
colors = M.fromList 	[   
				("comment", char $ color blue),
				("string", char $ color red),
				("reserved",char $ bold' $ color yellow),
				("float", char $ color bright_red),
				("Cons", char $ bold' $ color green),
				("plain", char $ color white)
			]

-- assign attributes to each character
highlight :: String -> Either String [Image]
highlight input = case pegMatch ( haskell M.! "all" )  input  of
			Right xs -> Right (map f xs)
			Left s	-> Left s -- should not happen .. ever
		where
--			if we have a type, go through the map
			f (c,(_:ty:_))	=	M.findWithDefault (char $ color white) ty colors $ c
--			otherwise default to black
			f (c,_)		=	(char $ color white) c

-- Test ...
main :: IO ()
main = do
	vty <- mkVty
	file <- S.readFile "Terminal.hs"
	let line = map ((foldr (<|>) empty_image).(\(Right x) -> x).highlight) $ lines file
        update vty $ pic_for_image $ foldr (<->) empty_image $ take 30 line
	loop line vty
        shutdown vty

loop file vty = do
			nextEV <- next_event vty
			case nextEV of
				EvKey ( KDown ) [] -> update vty ( pic_for_image $ foldr (<->) empty_image $ take 30 $   tail file ) >> loop (tail file) vty
				EvKey (KASCII q) [] -> return()
				_ -> loop file vty


