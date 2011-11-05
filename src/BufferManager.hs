module BufferManager(BM,currentBuffer,initBM) where
import qualified Buffer as B
import Data.Map(Map,(!),singleton,keys,insert)
import Data.Maybe(fromJust)
import System.IO
------ the datatype of buffer controllers ------ 

data BManager = BManager { 
                           buffers :: Map Int B.Buffer
                          ,curBuffer :: Int -- current buffer we're standing on
                          ,maxbuffer :: Int -- highest identifier given in a session
                         }

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
                           ,maxbuffer=1+maxbuffer bm },()))
--substitutes current buffer with the one given
modBuffer :: B.Buffer -> BM ()
modBuffer buf = BM (\bm -> (bm{buffers = insert (curBuffer bm) buf (buffers bm)},()))
--getting the current buffer from memory, searches with the currentbuffer as key
currentBuffer :: BM B.Buffer
currentBuffer = BM (\bm -> (bm, getCurBuff bm))
    where getCurBuff bm = (buffers bm)!(curBuffer bm)

--get the next buffer identifier and switch to next buffer
nextBuffer :: BM Int
nextBuffer = BM (\bm -> let allkeys = (keys.buffers) bm
                            hkeys = filter (> curBuffer bm) allkeys
                            finalkey   = if (not.null) hkeys then head hkeys else head allkeys in
                            (bm{curBuffer = finalkey},finalkey))

initBM :: BManager -- A ejecutarse al inicio del programa
initBM = BManager { buffers = singleton 0 B.newBuf
                  , curBuffer = 0, maxbuffer = 1 }

-- vi: et sw=4
