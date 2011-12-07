module ReadHighlighting where
import Peg.Peg
import Graphics.Vty hiding (Attr, (<|>) )
import Data.Map (Map, fromList)
import Data.Char
import Data.Monoid
import Control.Applicative

--	Documentation
--	Grammar:
--	 
--	read 		=	(definition ';')*
--	definition	=	' '* nombre ' '* '=' ' '* attr ' '* ( '|' ' '* attr  ' '*)*
--	attr 		=	"fgcolor" ' '+ color		
--			|	"bgcolor" ' '+ color
--			|	"bold"
--			|	"underline"
--			|	"dim"
--			|	"reverse"
--			|	"standout"
--
--	color		=	"black"
--			|	"white"
--			|	"cyan"
--			|	"magenta"
--			|	"blue"
--			|	"yellow"
--			|	"green"
--			|	"red"

data Attr	=	Attr
			{
				fgColor :: Maybe Color, -- foreground color
				bgColor :: Maybe Color, -- background color
				bold' :: Maybe (), -- is it bold ?
				underline' :: Maybe (),
				dim' :: Maybe (),
				reverse' :: Maybe (),
				standout' :: Maybe ()
			} deriving Show

instance Monoid Attr where
	mempty = Attr Nothing Nothing Nothing Nothing Nothing Nothing Nothing
	mappend b a =	Attr 
			{
				fgColor = fgColor a <|> fgColor b,
				bgColor = bgColor a <|> bgColor b,
				bold' = bold' a <|> bold' b,
				underline' = underline' a <|> underline' b,
				dim' = dim' a <|> dim' b,
				reverse' = reverse' a <|> reverse' b,
				standout' = standout' a <|> standout' b
			}

read' :: String -> Attr
read' s	|	s == "fblack"	=	mempty { fgColor = Just black}
	|	s == "fwhite"	=	mempty { fgColor = Just white}
	|	s == "fcyan"	=	mempty { fgColor = Just cyan}
	|	s == "fmagenta"	=	mempty { fgColor = Just magenta}
	|	s == "fblue"	=	mempty { fgColor = Just blue}
	|	s == "fyellow"	=	mempty { fgColor = Just yellow}
	|	s == "fgreen"	=	mempty { fgColor = Just green}
	|	s == "fred"	=	mempty { fgColor = Just red}
	|	s == "bblack"	=	mempty { bgColor = Just black}
	|	s == "bwhite"	=	mempty { bgColor = Just white}
	|	s == "bcyan"	=	mempty { bgColor = Just cyan}
	|	s == "bmagenta"	=	mempty { bgColor = Just magenta}
	|	s == "bblue"	=	mempty { bgColor = Just blue}
	|	s == "byellow"	=	mempty { bgColor = Just yellow}
	|	s == "bgreen"	=	mempty { bgColor = Just green}
	|	s == "bred"	=	mempty { bgColor = Just red}
	|	s == "bold"	=	mempty { bold' = Just () }
	|	s == "underline"=	mempty { underline' = Just ()}
	|	s == "dim"	=	mempty { dim' = Just () }
	|	s == "reverse"	=	mempty { reverse' = Just () }
	|	s == "standout"	=	mempty { standout' = Just () }
	|	otherwise	=	undefined -- now, don't use this bit.

--instance Monoid Attr where
--	mempty = Attr Nothing Nothing Nothing -- TODO
--	mappend a b = 


reader :: PegGrammar (Map String Attr)

--   PegAster   :: ([b] -> a)    -> (PegGrammar b) -> PegGrammar a
-- reader :: ([(String, [Attr])] -> Map String [Attr]) - > PegGrammar (String,[Attr]) -> ...
reader = PegCat head [ PegAster fromList definition , PegNegLA undefined (PegTerm undefined (const True)) ]

--   PegCat     :: ([b] -> a)    -> [PegGrammar b] -> PegGrammar a

--   PegEqToken :: (String -> a) -> String         -> PegGrammar a


spaces = PegAster undefined $ PegTerm undefined (and . map isSpace)

space = PegPlus undefined $ PegTerm undefined (and . map isSpace)

data Tri a b c = L a | M b | R c deriving Show

definition = PegCat ( \(_:(L s):_:_:_:(M att):_:(R xs):_ ) -> (s, mconcat (att:xs) ) )
		 [	spaces, name, spaces, PegTerm undefined (=="="),
				spaces, attr, spaces, PegAster R orAttr , PegTerm undefined (==";")]

name = PegPlus ((.) L concat) $ PegTerm id (/=[' '])

orAttr = PegCat (\(_:_:(M g):_) -> g) [ PegTerm undefined (=="|") , spaces, attr, spaces]

attr = PegAlt ((.) M read') $ 	[	
					PegCat (\(_:_:c:_) -> "f" ++ c) [ matchString "fgcolor" , space, color],
					PegCat (\(_:_:c:_) -> "b" ++ c) [ matchString "bgcolor" , space, color],
					PegCat head [style] 
				]

--   PegAlt     :: (b -> a)      -> [PegGrammar b] -> PegGrammar a


color = PegAlt id $ map matchString ["black", "white", "cyan", "magenta", "blue", "yellow", "green", "red"]

style = PegAlt id $ map matchString ["bold", "underline", "dim", "reverse", "standout"]

matchString str	= PegCat concat $ map (\s -> PegTerm id (==[s])) str
