module BufferManager(newBM,newBuffer,getLineBM,setLineBM,getBuffSizeBM,setYposBM) where
import qualified Buffer as B
import Data.Map(Map,(!),singleton,keys,insert,lookup,size,empty)
import Data.Maybe(fromJust)
import qualified Buffer as Buffer
import System.IO
import Prelude hiding (lookup)
------ the datatype of buffer controllers ------ 

data BManager = BManager { 
                           buffers :: Map Int B.Buffer
                          ,maxbuffer :: Int -- highest identifier given in a session
                         } deriving Show


--TODO: change all functions to actually do something
------ functions over BM ------

-- Constructor

newBM :: BManager
newBM = BManager empty 0

-- Creates a ne(BM,runBM,BManager,currentBuffer,nextBuffer,modBuffer,insBuffer,prevBuffer,initBM)w buffer 
newBuffer :: BManager -> Maybe FilePath -> IO (BManager, Int)
newBuffer bm Nothing	=	let bm' = insBuffer bm B.newBuf in return (bm', maxbuffer bm' - 1)
newBuffer bm (Just fp)	=	(>>=) (Buffer.readFile fp) $  \b -> let bm' = insBuffer bm b in return (bm', maxbuffer bm' - 1)


-- internal function
insBuffer :: BManager -> B.Buffer -> BManager
insBuffer bm buff	=	bm { buffers = insert (maxbuffer bm) buff buffer, maxbuffer = maxbuffer bm + 1}
	where
		buffer = buffers bm

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

--openFile :: BManager -> String -> BManager

