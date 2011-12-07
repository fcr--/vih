module Main where

import Terminal
import Data.Maybe
import BufferManager
import Graphics.Vty

main :: IO ()
main = mkVty >>= \vty ->
       let wtm = initWTM newBM vty in
       showWTM wtm