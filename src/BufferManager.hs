module BufferManager where
import qualified Buffer as B
import Data.Map(Map,(!),singleton,keys,insert,lookup,size,empty,delete,member)
import Data.Maybe(fromJust)
import qualified Buffer as Buffer
import System.IO
import Graphics.Vty(Image)
import Prelude hiding (lookup)
------ the datatype of buffer controllers ------ 
data BManager = BManager { 
                           buffers :: Map Int B.Buffer
                          ,maxbuffer :: Int -- highest identifier given in a session
                         } deriving Show


--TODO: change all functions to actually do something
------ functions over BM ------

-- Constructor, at the beginning ... there is always an empty buffer (i.e. one line, but empty)

newBM :: BManager
newBM = BManager (singleton 0 Buffer.newBuf) 1 

-- Creates a new buffer, cursor at position (0,0)
newBuffer :: BManager -> Maybe FilePath -> IO (BManager, Int)
newBuffer bm Nothing	=	let bm' = insBuffer bm B.newBuf in return (bm', maxbuffer bm' - 1)
newBuffer bm (Just fp)	=	(>>=) (Buffer.readFile fp) $  \b -> let bm' = insBuffer bm b in return (bm', maxbuffer bm' - 1)

openFileBM :: BManager -> Int -> String -> IO BManager
openFileBM bm bn fname = Buffer.readFile fname >>= \nb -> return $ bm{ buffers = insert bn nb (buffers bm)}

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

-- deleteLine :: Buffer -> Buffer
deleteLineBM :: BManager -> Int -> BManager
deleteLineBM bm bn = case lookup bn mp of
                            Just buff -> bm { buffers = insert bn ( Buffer.deleteLine buff) mp }
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
getXsizeBM :: BManager -> Int -> Int
getXsizeBM bm bn =	case lookup bn (buffers bm) of
				Just buff -> length $ Buffer.getLine buff
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


winUpBM :: BManager -> Int ->  BManager
winUpBM  bm bn = case lookup bn mp of
			Just buff -> bm { buffers = insert bn (Buffer.winUp buff) mp }
			Nothing -> undefined -- TODO .. good luck
	where
		mp = buffers bm

winDownBM :: BManager -> Int ->  BManager
winDownBM  bm bn = case lookup bn mp of
			Just buff -> bm { buffers = insert bn (Buffer.winDown buff) mp }
			Nothing -> undefined -- TODO .. good luck
	where
		mp = buffers bm

openLineBM :: BManager -> Int -> Bool -> BManager
openLineBM bm bn updown = case lookup bn (buffers bm) of
			Just buff -> bm {buffers = insert bn (Buffer.openLine buff updown) mp}
			Nothing -> undefined -- TODO ..good luck
	where
		mp = buffers bm

printWinBM :: BManager -> Int -> (Int,Int) -> (Image, BManager)
printWinBM bm bn pos = case lookup bn (buffers bm) of 
			Just buff -> let (img,buf) = Buffer.printBuff buff pos in (img, bm { buffers = insert bn buf mp} )
			Nothing -> undefined -- TODO ..good goddamn luck!
    where
        mp = buffers bm

writeFileBM :: BManager -> Int -> Maybe String -> IO BManager
writeFileBM bm bn mbstr = 
                    case lookup bn mp of
                        Just buff -> Buffer.writeFile mbstr buff >>= \b -> return $ bm {buffers = insert bn b mp}
                        Nothing -> undefined -- TODO ..good luck
    where
        mp = buffers bm
