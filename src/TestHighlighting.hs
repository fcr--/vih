import Peg.PegParser
import Peg.Peg
import qualified Data.Map as M


hola = parseGrammar "all = (comment  / string  / reserved (' ' / '\n' / !.) / Cons / float / plain / ' ' / '\n' )* ; comment = \"--\" (!'\n'.)*  ; string = '\"'(!'\"'.)*'\"' ;  reserved  = '|' / \"..\" / \"::\" / '@' /  '~' / \"->\" / \"<-\" /  words ; words = \"case\" / \"class\" / \"data\" / \"default\" / \"deriving\" / \"do\" / \"mdo\" / \"else\" / \"forall\" / \"foreign\" / \"hiding\" / \"if\" / \"import\" / \"in\" / \"infix\" / \"infixl\" / \"infixr\" / \"instance\" / \"let\" / \"module\" / \"newtype\" / \"of\" / \"qualified\" / \"then\" / \"where\" / \"type\" ; Cons = alphamay (alphamin / alphamay / num / \"'\"  / '_' / '#') * ; float = num+ ('.' num+)? ; plain = (!(num/' '/'\n').) (!(' '/'\n').)*; alphamin = 'a' / 'b' / 'c' / 'd' / 'e' / 'f' / 'g' / 'h' / 'i' / 'j' / 'k' / 'l' / 'm' / 'n' / 'o' / 'p' / 'q' / 'r' / 's' / 't' / 'u' / 'v' / 'w' / 'x' / 'y' / 'z' ; alphamay = 'A' / 'B' / 'C' / 'D' / 'E' / 'F' / 'G' / 'H' / 'I' / 'J' / 'K' / 'L' / 'M' / 'N' / 'O' / 'P' / 'Q' / 'R' / 'S' / 'T' / 'U' / 'V' / 'W' / 'X' / 'Y' / 'Z' ; num = '1' / '2' / '3' / '4' / '5' / '6' / '7' / '8' / '9'/ '0' ; "

test = pegMatch ( hola M.! "all" )  "case x of True -> xD" 



{-

FORMA LEGIBLE

hola = parseGrammar  "all =  (comment / string / reserved / Cons / float / plain)*  ; string = '\"'(!'\"'.)*'\"' ;

comment = \"--\" (!'\n'.)*  ;    FALTA MULTILINEA

string = '\"'(!'\"'.)*'\"' ;

reserved  = '|' / \"..\" / \"::\" / '@' /  '~' / \"->\" / \"<-\" /  words ;

words = \"case\" / \"class\" / \"data\" / \"default\" / \"deriving\" / \"do\" / \"mdo\" / \"else\" / \"forall\" / \"foreign\" / \"hiding\" / \"if\" / \"import\" / \"in\" / \"infix\" / \"infixl\" / \"infixr\" / \"instance\" / \"let\" / \"module\" / \"newtype\" / \"of\" / \"qualified\" / \"then\" / \"where\" / \"type\"

Cons = alphamay (alphamin / alphamay / num / ' / _ / #)*

float = num+ ('.' num+)?


alphamin = 'a' / 'b' / 'c' / 'd' / 'e' / 'f' / 'g' / 'h' / 'i' / 'j' / 'k' / 'l' / 'm' / 'n' / 'o' / 'p' / 'q' / 'r' / 's' / 't' / 'u' / 'v' / 'w' / 'x' / 'y' / 'z' ;

alphamay = 'A' / 'B' / 'C' / 'D' / 'E' / 'F' / 'G' / 'H' / 'I' / 'J' / 'K' / 'L' / 'M' / 'N' / 'O' / 'P' / 'Q' / 'R' / 'S' / 'T' / 'U' / 'V' / 'W' / 'X' / 'Y' / 'Z' ;

num = '1' / '2' / '3' / '4' / '5' / '6' / '7' / '8' / '9' / '0' ;

-}
