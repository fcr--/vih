module Main where
import qualified Graphics.Vty V
import Data.Map(Map,(!),singleton,keys,insert)

------ Data definitions ------

--Data holder of a windows tiling manager
data WTManager = WTM {lo :: Layout -- current layout
                     ,curwdw :: Position -- current window being used
                      wtmH :: Int -- height in characters of window
                      wtmW :: Int -- width in characters of window
                     }

newtype WTM a = WTM{runWTM :: WTManager -> (WTManager, a)
instance monad WTM where
 --(>>=) :: WTM a -> (a -> WTM b) -> WTM b
 
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
--
data Layout = Vspan Int Int [Layout,Int] --Vertical span with sizes
             |Hspan Int Int [Layout,Int] --Horizontal span with sizes
             |Window Int Int Int -- Window with buffer number

--To navigate through the layout
newtype Position = [Int]
main :: IO ()
main = mkVty >>= 
