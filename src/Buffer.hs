module Buffer where

import qualified System.IO as S (readFile)

data Buffer = Buffer {
    contents :: ([BufferLine], BufferLine, [BufferLine]),
    curLine  :: Int,
    numLines :: Int} -- todo: add missing stuff
  deriving Show

data BufferLine = BufferLine String -- TODO: add more attrs
  deriving Show

------ Buffer functions ------

readFile :: FilePath -> IO Buffer

readFile fn = fmap (load . noNull . lines) $ S.readFile fn
  where
  noNull ls = if null ls then [""] else ls
  load :: [String] -> Buffer
  load ls = Buffer { contents = ([], head lines, tail lines),
		     curLine = 0, numLines = length lines }
    where
    lines = map loadLine ls

-- empty buffer constructor

newBuf :: Buffer
newBuf = Buffer { contents = ([], BufferLine [],[]), curLine = 0, numLines = 1}

-- writeFile :: FilePath -> IO Buffer

lineUp :: Buffer -> Buffer
lineUp buff
  | curLine buff == 0  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine }
    where
    newContents = (\(p,c,n)-> (init p, last p, c:n)) $ contents buff
    newCurLine = curLine buff - 1

lineDown :: Buffer -> Buffer
lineDown buff
  | curLine buff == numLines buff - 1  = buff
  | otherwise          = buff { contents = newContents, curLine = newCurLine }
    where
    newContents = (\(p,c,n)-> (p++c, head n, tail n)) $ contents buff
    newCurLine = curLine buff + 1

getLine :: Buffer -> BufferLine
getLine buff = let (p,c,n) = contents buff in c

-- updateLine :: Buffer -> BufferLine -> Buffer

-- deleteLine :: Buffer -> Buffer

-- equivalent to pressing 'O' (false) or 'o' (true).
-- openLine :: Buffer -> Bool -> Buffer
-- openLine buff after = ...

-- equivalent to pressing J in normal mode of vim.
-- joinLine :: Buffer -> Buffer


------ BufferLine functions ------

loadLine :: String -> BufferLine
loadLine = BufferLine
