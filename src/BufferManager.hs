module BufferManager(BM,runBM,BManager,currentBuffer,nextBuffer,modBuffer,insBuffer,prevBuffer,initBM) where
import qualified Buffer as B
import Data.Map(Map,(!),singleton,keys,insert,lookup,size)
import Data.Maybe(fromJust)
import qualified Buffer as Buffer
import System.IO
import Prelude hiding (lookup)
------ the datatype of buffer controllers ------ 

data BManager = BManager { 
                           buffers :: Map Int B.Buffer
                          ,curBuffer :: Int -- current buffer we're standing on
                          ,maxbuffer :: Int -- highest identifier given in a session
                          ,vNumber :: Int -- version number: updates every time the BM is modified
                         } deriving(Show)

newtype BM a = BM {runBM :: BManager -> (BManager, a)} -- monad hiding all this
instance Monad BM where
 --(>>=) :: BM a -> (a -> BM b) -> BM b 
 b >>= c = BM (\bm-> let (interBM,a) = runBM b bm in
                 (runBM (c a)) interBM )
 --return :: a -> BM a               
 return a = BM (\bm -> (bm,a))

--TODO: change all functions to actually do something
------ functions over BM ------


--adding a buffer object to the BM, addds 1 to maxbuffer, adds the buffer to the bufferdata list
insBuffer :: B.Buffer -> BM ()
insBuffer newbuf = BM (\bm->(bm {buffers = insert (maxbuffer bm) newbuf (buffers bm) 
                           ,maxbuffer=1+maxbuffer bm,vNumber = 1+vNumber bm},()))

--substitutes current buffer with the one given
modBuffer :: B.Buffer -> BM ()
modBuffer buf = BM (\bm -> (bm{buffers = insert (curBuffer bm) buf (buffers bm),vNumber = 1+vNumber bm},()))

--getting the current buffer from memory, searches with the currentbuffer as key
currentBuffer :: BM B.Buffer
currentBuffer = BM (\bm -> (bm {vNumber = 1+vNumber bm}, getCurBuff bm))
    where getCurBuff bm = (buffers bm)!(curBuffer bm)

--get the next buffer identifier and switch to next buffer
nextBuffer :: BM Int
nextBuffer = BM (\bm -> let allkeys = (keys.buffers) bm
                            hkeys = filter (> curBuffer bm) allkeys
                            finalkey   = if (not.null) hkeys then head hkeys else head allkeys in
                            (bm{curBuffer = finalkey, vNumber = 1+vNumber bm},finalkey))

--get the previous buffer identifier and switch to the previous buffer
prevBuffer :: BM Int
prevBuffer = BM (\bm -> let allkeys = (keys.buffers) bm
                            lkeys = filter (< curBuffer bm) allkeys
                            finalkey = if (not.null) lkeys then (head.reverse) lkeys else (head.reverse) allkeys in
                            (bm{curBuffer = finalkey, vNumber = 1+vNumber bm},finalkey))

--switch to arbitrary buffer and return True if the buffer existed
swBuffer :: Int -> BM Bool
swBuffer arbBuff = BM (\bm -> let (nb,i) = case (lookup arbBuff.buffers) bm of
                                        Nothing -> (curBuffer bm, 0)
                                        (Just b)-> (arbBuff, 1) 
                              in
                              (bm{curBuffer = nb,vNumber = i+vNumber bm},curBuffer bm == nb))

initBM :: BManager -- Creates a new single buffer with no associated file
initBM = BManager { buffers = singleton 0 B.newBuf
                  , curBuffer = 0, maxbuffer = 1, vNumber = 0}

-- vi: et sw=4

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


