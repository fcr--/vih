module Buffer where
import Peg.PegParser
import Graphics.Vty
import qualified Data.Map as M
import qualified System.IO as S (readFile,writeFile)

data Buffer = Buffer {
	contents :: ([BufferLine], BufferLine, [BufferLine]),
	curLine  :: Int,
	numLines :: Int,
	grammar  :: Defs,
	colors   :: M.Map String (Char -> Image) -- TODO : ...
	} -- TODO: Cursor y atributos

data BufferLine = BufferLine	{  line :: String -- , memo :: [Image]   TODO: tabla de memoization...
				}
-- TODO : show
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
readFile fn = fmap (load . noNull . lines) $ S.readFile fn
  where
  noNull ls = if null ls then [""] else ls
  load :: [String] -> Buffer
  load ls = Buffer { contents = ([], head lines, tail lines),
		     curLine = 0, numLines = length lines, grammar = M.empty, colors = M.empty }
    where
    lines = map loadLine ls

-- empty buffer constructor

newBuf :: Buffer
newBuf = Buffer { contents = ([], loadLine [] ,[]), curLine = 0, numLines = 1, grammar = M.empty, colors = M.empty}

-- writeFile :: FilePath -> IO Buffer
writeFile :: FilePath -> Buffer -> IO ()
writeFile fp buf = S.writeFile fp (txt buf)
 where txt :: Buffer -> String
       txt b = let (pr,c,n) = contents b in foldr1 (\x y -> x ++ "\n" ++ y) $ map unLoadLine ( reverse pr ++ [c] ++ n)

firstLine :: Buffer -> Buffer
firstLine buff
  | curLine buff == 0 = buff
  | otherwise         = buff { contents = newContents, curLine = 0}
    where
    newContents = (\(p,c,n) -> ([], last p, init p ++ [c] ++ n)) $ contents buff

lastLine :: Buffer -> Bool
lastLine buff
  | curLine buff == numLines buff - 1 = True
  | otherwise                         = False

lineUp :: Buffer -> Buffer
lineUp buff
  | curLine buff == 0  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine }
    where
    newContents = (\(p,c,n)-> (tail p, head p, c:n)) $ contents buff
    newCurLine = curLine buff - 1

lineDown :: Buffer -> Buffer
lineDown buff
  | curLine buff == numLines buff - 1  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine }
    where
    newContents = (\(p,c,n)->   (c : p , head n, tail n)) $ contents buff
    newCurLine = curLine buff + 1

getLine :: Buffer -> BufferLine
getLine buff = let (p,c,n) = contents buff in c


-- TODO : Highlighting for a give buffer.
-- colorLine :: Buffer -> [Image]

updateLine :: Buffer -> BufferLine -> Buffer
updateLine buff cr	=	let (p,_,s) = contents buff in	buff { contents = (p,cr,s) }

-- dd
deleteLine :: Buffer -> Buffer
deleteLine buff		 	|	nl == 1		=	buff {contents =  ([],loadLine "",[]) }
				|	null sig	=	buff { contents = ( tail pr, head pr, sig), curLine = (cl-1), numLines =  (nl-1)}
				|	otherwise	=	buff { contents =  ( pr, head sig, tail sig), curLine =  cl, numLines =  (nl-1)}
	where
		cl = curLine buff
		nl = numLines buff
		(pr,l,sig) = contents buff

-- equivalent to pressing 'O' (false) or 'o' (true).
openLine :: Buffer -> Bool -> Buffer
openLine buff after 	|	after		=	buff { contents = oT (contents buff), curLine = (curLine buff + 1), numLines = (numLines buff + 1) }
			|	otherwise	=	buff { contents = oF (contents buff), numLines = (numLines buff + 1) }
	where
		oT	=	\(prev, l, sig) -> ( l : prev, loadLine "", sig)
		oF	=	\(prev, l, sig) -> ( prev , loadLine "", l : sig)

-- equivalent to pressing J in normal mode of vim.
joinLine :: Buffer -> Buffer
joinLine buff	=	doIt (contents buff)
	where
		doIt	= \(prev, l@(BufferLine ls ), sig) -> case sig of
--						Non-empty line, add a space in the middle
						(BufferLine s@(_:_) ):ss -> buff { contents = (prev, loadLine (ls ++ (' ':s) ), ss) , numLines = (numLines buff - 1) }
--						Empty line, consume line
						(_:ss) -> buff { contents = (prev, l, ss) , numLines = (numLines buff - 1) }
--						No next line... nothing
						_		  -> buff

------ BufferLine functions ------

loadLine :: String -> BufferLine
loadLine s = BufferLine s -- []

unLoadLine :: BufferLine -> String
unLoadLine bl = line bl
