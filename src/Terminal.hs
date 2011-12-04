module Main where
import BufferManager(BManager,BM,initBM)
import Control.Concurrent.STM
import Data.Map(Map,(!),singleton,keys,insert)
import Graphics.Vty
import Data.Word
------ Data definitions ------

--Data holder of a windows tiling manager
--Except the very last line, the whole terminal is described by the layout (except maybe overlays)
data WTManager = WTMa {lo :: Layout -- current layout
                      ,curwdw :: [Int] -- current window being used
                      ,wtmH :: Int -- height in characters of window
                      ,wtmW :: Int -- width in characters of window
                      ,bm :: BManager
                      }


newtype WTM a = WTM{runWTM :: WTManager -> (WTManager, a)}
instance Monad WTM where
 --(>>=) :: WTM a -> (a -> WTM b) -> WTM b
 wtm >>= op = WTM (\wtma -> let (interWTM,a) = runWTM wtm wtma in
                      runWTM (op a) interWTM)
 return a = WTM (\wtm -> (wtm,a))
 
 -- The way to initialize the window manager at program startup
 -- TODO: change wsizes, Window initializer, possibly add a startup
 -- size variable
initWTM :: BManager -> WTManager
initWTM ref = WTMa {lo = NoWin
                        ,curwdw = [0]
                        ,wtmH = 0
                        ,wtmW = 0
                        ,bm = ref
                        }

--Data representing current tiling. example:
--
-- Hspan 14 9 [Vspan 5 5 [Window 3 3 b1,Window 3 2 b2],Window 8 6 b3]
--
-- 12345678901234
-- **************
-- *1  *1       *
-- * 2 * 2      *
-- *  3*  3     *
-- *****   4    *
-- *1  *    5   *
-- * 23*     678*
-- **************
-- *commandline *

-- Layout explanation:
-- Cons X Y Sth:
--      X = horizontal span of the component
--      Y = vertical span of the component
--      Sth = content of the component
-- (H)Vspan = X Y [(Cp,Int)] = v/h span of component, plus spans of subcomponents
--
data Layout = Vspan Int Int [(Layout,Int)] --Vertical span with sizes
             |Hspan Int Int [(Layout,Int)] --Horizontal span with sizes
             |Window Int Int Int -- Window with buffer number
             |NoWin
--To navigate through the layout
bgcolor = def_attr
dum = def_attr

main :: IO ()
main = mkVty >>= \vty -> 
        let wtm = initWTM initBM in
		next_event vty >>= \k -> 
        display_bounds ( terminal vty ) >>= \(DisplayRegion w h) ->
		printloop vty wtm w h >>
		shutdown vty

printloop :: Vty -> WTManager -> Word -> Word -> IO ()
printloop vty wtm w h= do
                    update vty (pic_for_image $ armarIm w h wtm)
                    nextEV <- next_event vty
                    case nextEV of 
                        EvKey (KASCII 'q') [] -> return ()
                        EvResize nx ny -> printloop vty wtm (fromIntegral nx) (fromIntegral ny)
                        _ -> return ()

armarIm w h wtm =   bordeSuperior w
                 <->(printWTM w (h-3) wtm)
                 <->bordeInferior w


bordeSuperior w = char_fill dum '-' w 1
bordeInferior w = char_fill dum '-' w 2
printWTM w h wtm = char_fill dum ' ' w h
                
--updateThread :: TVar BManager -> IO ()
--updateThread v state = do
--  state' <- doSomething state
--  updateThread v state'

