module BufferManager where
import qualified Buffer as B
import Data.Map(Map,(!),singleton,keys,insert,lookup,size,empty,delete,member)
import Data.Maybe(fromJust)
import qualified Buffer as Buffer
import System.IO
import Prelude hiding (lookup)
------ the datatype of buffer controllers ------ 

data BManager = BManager { 
                           buffers :: Map Int B.Buffer
                          ,maxbuffer :: Int -- highest identifier given in a session
--                          ,mystery :: Map Int Int
                         } deriving Show


--TODO: change all functions to actually do something
------ functions over BM ------

-- Constructor, at the beginning ... there is always an empty buffer (i.e. one line, but empty)

newBM :: BManager
newBM = BManager (singleton 0 Buffer.newBuf) 1  -- empty -- PARA mystery

-- Creates a new buffer, cursor at position (0,0)
newBuffer :: BManager -> Maybe FilePath -> IO (BManager, Int)
newBuffer bm Nothing	=	let bm' = insBuffer bm B.newBuf in return (bm', maxbuffer bm' - 1)
newBuffer bm (Just fp)	=	(>>=) (Buffer.readFile fp) $  \b -> let bm' = insBuffer bm b in return (bm', maxbuffer bm' - 1)


-- internal function
insBuffer :: BManager -> B.Buffer -> BManager
insBuffer bm buff	=	bm { buffers = insert n buff buffer, maxbuffer = n + 1}
	where
		n = maxbuffer bm
		buffer = buffers bm	

-- deletes a given buffer (chosen by number)
deleteBuffer :: BManager -> Int -> BManager
deleteBuffer bm bn	=	bm { buffers = delete bn (buffers bm)}

-- Number of buffers.
getBuffSizeBM :: BManager  -> Int
getBuffSizeBM bm  = size $ buffers bm


-- BManager -> Buffer Number -> Line Number -> (New BManager, String)
getLineBM :: BManager -> Int -> Int -> String
getLineBM bm bn line = case lookup bn (buffers bm) of
                            Just buff -> Buffer.getLine $ Buffer.setY line buff
                            Nothing   -> undefined -- TODO ..good luck
	where
		mp = buffers bm

-- TODO : ESTRUCTURA MEJOR.
-- BManager -> Buffer Number -> Line Number -> String -> ...
setLineBM :: BManager -> Int -> Int -> String -> BManager
setLineBM bm bn line s = case lookup bn mp of
                            Just buff -> bm { buffers = insert bn ( Buffer.setY (Buffer.getLineNumber buff) (Buffer.setLine s $ Buffer.setY line buff) ) mp }
                            Nothing   -> undefined -- TODO ..good luck
	where
		mp = buffers bm

-- BManager -> Buffer Number -> Line Number -> BManager
setYposBM :: BManager -> Int -> Int -> BManager
setYposBM bm bn line = case lookup bn mp of
                            Just buff -> bm { buffers = insert bn (Buffer.setY line buff) mp }
                            Nothing   -> undefined -- TODO ..good luck
	where
		mp = buffers bm

getYposBM :: BManager -> Int -> Int
getYposBM bm bn = case lookup bn (buffers bm) of
			Just buff -> Buffer.getY buff
			_ -> undefined -- TODO ..good luck
 
getYsizeBM :: BManager -> Int -> Int
getYsizeBM bm bn	=	case lookup bn (buffers bm) of
					Just buff -> Buffer.getSize buff
					Nothing -> undefined  -- TODO ..good luck

-- settes and getters of cursors

getXposBM :: BManager -> Int -> Int
getXposBM bm bn = case lookup bn (buffers bm) of
			Just buff  -> Buffer.getX buff
			Nothing -> undefined -- TODO ..good luck

-- BManager -> Buffer Number -> Position -> New buffer manager
setXposBM :: BManager -> Int -> Int -> BManager
setXposBM bm bn newpos = case lookup bn mp of
			Just buff  -> bm {buffers = insert bn (Buffer.setX newpos buff) mp}
			Nothing -> undefined -- TODO ..good luck
	where
		mp = buffers bm
