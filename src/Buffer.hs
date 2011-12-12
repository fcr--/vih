module Buffer where
import Peg.PegParser hiding (char, string)
import Peg.Peg 
import Graphics.Vty
import Debug.Trace -- para jorobar
import qualified TestHighlighting as TestHighlighting (highlight)
import qualified Data.Map as M
import qualified System.IO as S (readFile,writeFile)
import System.Directory(getAppUserDataDirectory)
import System.FilePath(joinPath)
import qualified ReadHighlighting as RH
import Data.Monoid

-- TODO : TABS!!!!

data Buffer = Buffer {
	contents :: ([BufferLine], BufferLine, [BufferLine]),
	curLine  :: Int,
	curPos	 :: Int, -- position of the cursor within the line curLine
	numLines :: Int,
	grammar  :: Defs,
	colors   :: M.Map String RH.Attr,
	file	 :: Maybe FilePath, -- file associated with the buffer
	winPoint :: Int -- window pointer
	} 

-- highlighting with memoization
data BufferLine = BufferLine	{  line :: String , memo :: [Image] } 

-- For debugging
instance Show Buffer where
	show buff = "Buffer (" ++ show pr ++ ", " ++ show c ++ ", " ++ show sig ++ ") " ++ show cr ++ " " ++ show nl
		where
			(pr,c,sig) = contents buff
			cr = curLine buff
			nl = numLines buff

instance Show BufferLine where
	show buff = "Bufferline " ++ show s
		where
			s = line buff

------ Buffer functions ------

readFile :: FilePath -> IO Buffer
readFile fn = do
		(df,mp) <- catch (readGrammar ext) ( const $ return (emptyGrammar,M.empty) ) 
		file <- catch (S.readFile fn) (const $ return "")
		let buff = (load . noNull . lines) $ concat $ map (\s -> if s=='\t' then take 8 (repeat ' ') else [s] ) $ file -- TODO : TABS TO 8 spaces
		let b1 = buff { grammar = df, colors = mp }
		let (p,c,s) = contents b1
--		associate the buffer with the file.
		return $ b1 {contents = (map (func b1) p, (func b1) c, map (func b1) s) , file = Just fn } -- colors! ;D
  where
  func b1 = (\(BufferLine s _) -> loadLine b1 s)
  noNull ls = if null ls then [""] else ls
  ext = (\xs -> if null xs then xs else tail xs) . snd . break (=='.') $ fn 
  load :: [String] -> Buffer
  load ls = Buffer { contents = ([], head lines, tail lines),
		     curLine = 0, curPos = 0, numLines = length lines, grammar = emptyGrammar, colors = M.empty , file = Nothing, winPoint = 0 }
    where
    lines = map (\s -> BufferLine s []) ls -- no colors yet, wait for the file extension

emptyGrammar = parseGrammar "all = .* ; "  -- always matches, but the colors are always default

-- empty buffer constructor

newBuf :: Buffer
newBuf = let ans = Buffer { contents = ([], loadLine ans [] ,[]), curLine = 0, curPos = 0, numLines = 1, grammar = emptyGrammar , colors = M.empty, file = Nothing, winPoint =  0 } in ans

-- writeFile :: FilePath -> IO Buffer
writeFile :: Maybe FilePath -> Buffer -> IO Buffer
writeFile Nothing buf = case file buf of
				Nothing -> return buf -- failed
				Just file -> writeFile' file buf

writeFile (Just file) buf = writeFile' file buf

writeFile' fp buf = do
			(df,mp) <- catch (readGrammar ext) ( const $ return (emptyGrammar,M.empty) )
			catch (S.writeFile fp (txt buf)) (const $ return ()) 
			return $ buf { grammar = df, colors = mp, file = Just fp} -- associate the buffer with the file.
 where txt :: Buffer -> String
       txt b = let (pr,c,n) = contents b in foldr1 (\x y -> x ++ "\n" ++ y) $ map unLoadLine ( reverse pr ++ [c] ++ n)
       ext = (\xs -> if null xs then xs else tail xs) . snd . break (=='.') $ fp


firstLine :: Buffer -> Buffer
firstLine buff
  | curLine buff == 0 = buff
  | otherwise         = buff { contents = newContents, curLine = 0}
    where
    newContents = (\(p,c,n) -> ([], last p, reverse (init p) ++ [c] ++ n)) $ contents buff

lastLine :: Buffer -> Bool
lastLine buff
  | curLine buff == numLines buff - 1 = True
  | otherwise                         = False

lineUp :: Buffer -> Buffer
lineUp buff
  | curLine buff == 0  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine , winPoint = winPoint buff - 1}
    where
    newContents = (\(p,c,n)-> (tail p, head p, c:n)) $ contents buff
    newCurLine = curLine buff - 1

lineDown :: Buffer -> Buffer
lineDown buff
  | curLine buff == numLines buff - 1  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine , winPoint = winPoint buff + 1}
    where
    newContents = (\(p,c,n)->   (c : p , head n, tail n)) $ contents buff
    newCurLine = curLine buff + 1

-- dd
deleteLine :: Buffer -> Buffer
deleteLine buff		 	|	nl == 1		=	buff { contents =  ([],loadLine buff "",[]) }
				|	null sig	=	buff { contents = ( tail pr, head pr, sig), curLine = (cl-1), numLines =  (nl-1)}
				|	otherwise	=	buff { contents =  ( pr, head sig, tail sig), curLine =  cl, numLines =  (nl-1)}
	where
		cl = curLine buff
		nl = numLines buff
		(pr,l,sig) = contents buff

-- equivalent to pressing 'O' (false) or 'o' (true).
openLine :: Buffer -> Bool -> Buffer
openLine buff after 	|	after		=	buff { contents = oT (contents buff), curLine = (curLine buff + 1), numLines = (numLines buff + 1) , winPoint = winPoint buff + 1} -- actually this should depend upon the dimensions of the terminal
			|	otherwise	=	buff { contents = oF (contents buff), numLines = (numLines buff + 1) }
	where
		oT	=	\(prev, l, sig) -> ( l : prev, loadLine buff "", sig)
		oF	=	\(prev, l, sig) -> ( prev , loadLine buff "", l : sig)

-- equivalent to pressing J in normal mode of vim.
joinLine :: Buffer -> Buffer
-- joinLine buff | trace (show $ numLines buff) False = undefined
joinLine buff	=	doIt (contents buff)
	where
		doIt	= \(prev, l@(BufferLine ls _), sig) -> case sig of
--						Non-empty line, add a space in the middle
						(BufferLine s@(_:_) _):ss -> buff { contents = (prev, loadLine buff (ls ++ (' ':s) ), ss) , numLines = (numLines buff - 1) }
--						Empty line, consume line
						(_:ss) -> buff { contents = (prev, l, ss) , numLines = (numLines buff - 1) }
--						No next line... nothing
						_		  -> buff

------ BufferLine functions ------


-- TODO : Check RIGHT's
-- does the highlighting of the new line
--loadLine :: String -> BufferLine
--loadLine s = BufferLine s $ (\(Right x) -> x) $ (TestHighlighting.highlight s) -- []

loadLine :: Buffer -> String -> BufferLine
loadLine buff s = BufferLine s  $ highlight' buff s

unLoadLine :: BufferLine -> String
unLoadLine bl = line bl


highlight :: Buffer -> ([[Image]],[Image],[[Image]])
-- Annoying a bit...
--highlight buff |	trace "FALTA LO DE EXTRAER LA GRAMÁTICA DE UN ARCHIVO" False	= undefined
-- we are not re-computing the highlighting here, just extracting the memoized value.
highlight buff = let (p,c,s) = contents buff in (map memo p, memo c, map memo s)

-- TODO: FINISH THIS FUNCTION... 
-- windowPointer indicates the number of visual lines to be printed, above from the visual lines associated witht the current line
-- from Buffer, windowPointer (corresponding to this buffer), (weight ;P , height) ... to (Image, new wp)
-- TODO: Probar con la función main de este módulo en ghci.
printBuff :: Buffer -> Bool  -> (Int,Int) -> (Image,Buffer)
printBuff buf cursor (w,h)	|	d > h		=	error "printBuff" -- ver en qué sublínea está el cursor
				|	otherwise	=	(vert_cat $ take h $ reverse ( take wp' p' )  ++ l' ++ s' ++ noqui , buf { winPoint =  wp'})
	where
		noqui = repeat (string def_attr "~")
		wp = winPoint buf
		wp' = min' 0 p' $ min wp (h-d)
		(p,l,s) = highlight buf
		func = map horiz_cat
		p' = concat $ map (reverse.func.partition') p -- [Image]
		s' = concat $ map (func.partition') s -- [Image]
--		l' = func $ partition' l -- images of the lines [Image]
		x' = getX buf -- max 0 $ min (length( Buffer.getLine buf) - 1 ) $ getX buf
		l' = if cursor then func $ partition' $ take x' l ++ map (char ((def_attr `with_style` blink) `with_style` reverse_video)) cursor_block ++ drop (x' + 1) l else func $ partition' $ l  -- images of the lines [Image]
		cursor_block = let d = drop x' l in if null d then " " else (flip replicate ' ' $ fromEnum $ image_width $ head d)
		d  = length l'
		empty_line = take w $ repeat (char def_attr ' ')
		partition' :: [Image] -> [[Image]] -- line to list of "cropped" lines
		partition'  []	=	empty_line : []
		partition'  xs	=	let ys = drop' w xs in case null ys of
								True -> if length xs == w then xs : [] else (xs ++ take' (w - length xs) (repeat $ char def_attr ' ')) : []
								_ -> take' w xs : partition' ys
                take' w [] = []
                take' w (x:xs) = if xw <= w then x:take' (w-xw) xs else []
                    where xw = fromEnum $ image_width x
                drop' w [] = []
                drop' w (x:xs) = if xw <= w then drop' (w-xw) xs else (x:xs)
                    where xw = fromEnum $ image_width x
		min' acc [] _ = acc -- to prevent wp' from being larger than the number of lines that are above the current one.
		min' acc (_:xs) n	|	n <= 0 		=	 acc
					|	otherwise	=	 min' (acc+1) xs (n-1)

{- let (lup,resto) = divMod (y-1) 2
			  (pre,line,post) = highlight buf
			  (pr',li',po') = (take lup pre, line ++ (take (x-length line) ++ take (repeat [char def_attr ' ']),take (lup+resto) post)
			  (pr'',li'',po'') = (map horiz_cat (map take x pr'),map horiz_cat ( map (take x) li'),map horiz_cat (li', ( map (take x) po'))
			  in = vert_cat [vert_cat (reverse pr''),li'',vert_cat po''] 
			  \(pre,line,post)-> (take lup (reverse pre))++) ((\f (plp) (take y) (highlight buf))

dummyLine w = char def_attr '' w
-}
highlight' :: Buffer -> String -> [Image]
highlight' buff s	=	map f $ (\(Right x) -> x) $ pegMatch (gram M.! "all") s
	where
		gram = grammar buff
		col = colors buff
		f (c,xs) = (char $ RH.attr2VtyAttr $ mconcat $ map (\x -> M.findWithDefault mempty x col) xs ) c

------ Various functions ------

getSize :: Buffer -> Int
getSize buff = numLines buff

-- Get the contents of the currennt line

getLine :: Buffer -> String
getLine buff = let (p,c,n) = contents buff in unLoadLine c

-- Set the contents of the current line
setLine :: String -> Buffer -> Buffer
setLine cr buff	=	let (p,_,s) = contents buff in	buff { contents = (p,loadLine buff cr,s) }

-- Get the line number of the cursor
getLineNumber :: Buffer -> Int
getLineNumber buff = curLine buff

-- TODO : Check whether this is okay 
getX :: Buffer -> Int
getX buff = max 0 $ min l  $ curPos buff
	where	l =  (length $ Buffer.getLine buff)

setX :: Int -> Buffer -> Buffer
setX x buff	=	buff { curPos = x}

-- Respecto al 0
getY :: Buffer -> Int
getY buff = curLine buff

-- Respecto al 0
setY :: Int -> Buffer  -> Buffer
setY y buff	=	find cl (contents buff) 
	where
		fy = max 0 $ min ((numLines buff) - 1) y
		cl = curLine buff
		find pos cont@(pr,c,sig) --	|	trace (show pos ++ " " ++ show c) False	= undefined
						|	pos > fy	= find (pos-1) ( tail pr, head pr, c : sig)
						|	pos < fy	= find (pos+1) ( c:pr , head sig, tail sig)
 						|	otherwise 	= buff { contents = cont , curLine = fy, winPoint = winPoint buff + fy - cl }

winUp :: Buffer -> Buffer
winUp buff = buff { winPoint = winPoint buff + 1}

winDown :: Buffer -> Buffer
winDown buff = buff { winPoint = winPoint buff - 1 } 

--	Read Grammar

-- Extension -> (Grammar, Attributes for non-terminals)
-- hs.peg , hs.attr
-- all <- el "primer" no terminal.
readGrammar :: String -> IO (Defs,M.Map String RH.Attr)
readGrammar s  = do
			home <- getAppUserDataDirectory "vih"  -- = undefined
			gram <- S.readFile ( joinPath [home ,(s ++ ".peg")] )
			high <- S.readFile ( joinPath [home ,(s ++ ".attr")] )
			case pegMatch RH.reader $ filter (/='\n') high of
				Left s ->  return (emptyGrammar, M.empty)
				Right x -> return ( parseGrammar gram  , x) 
			-- TODO hopefully it will always be right!


-- test
{-
main :: IO ()
main =	 do
		vty <- mkVty
		buffer <- Buffer.readFile "ReadHighlighting.hs"
		case printBuff (buffer {curPos = 10}) (80,24) of -- window up
				(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
		shutdown vty
	where
		sp xs = if null xs then [(char def_attr ' ')] else xs

loop buffer vty = do
			nextEV <- next_event vty
			case nextEV of
				EvKey ( KASCII 'u' ) []  -> case printBuff (buffer { winPoint = winPoint buffer + 1}) (80,24) of -- window up
								(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
				EvKey ( KASCII 'y' ) []  -> case printBuff (buffer { winPoint = winPoint buffer - 1}) (80,24) of -- window down
								(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
				EvKey ( KASCII 'q' ) []  -> return()
				EvKey ( KASCII 'a' ) []  -> case printBuff bu (80,24) of
								(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
				EvKey ( KASCII 'j' ) [] -> case printBuff bj (80,24) of
								(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
				_ -> case printBuff bd (80,24) of
					(img,buf) -> ( update vty $ pic_for_image $ img ) >> loop buf vty
	where
		bd = ( lineDown buffer )
		bu = (lineUp buffer)
		bj = joinLine buffer -}
