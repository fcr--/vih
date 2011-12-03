module Main where
import BufferManager(BManager,BM)
import Control.Concurrent.STM
import Data.Map(Map,(!),singleton,keys,insert)

------ Data definitions ------

--Data holder of a windows tiling manager
--Except the very last line, the whole terminal is described by the layout (except maybe overlays)
data WTManager = WTMa {lo :: Layout -- current layout
                      ,curwdw :: [Int] -- current window being used
                      ,wtmH :: Int -- height in characters of window
                      ,wtmW :: Int -- width in characters of window
                      ,bm :: TVar BManager
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
initWTM :: TVar BManager -> WTManager
initWTM ref = WTMa {lo = Window 0 0 0
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

--To navigate through the layout
main :: IO ()
main = putStrLn "hola"

--updateThread :: TVar BManager -> IO ()
--updateThread v state = do
--  state' <- doSomething state
--  updateThread v state'
