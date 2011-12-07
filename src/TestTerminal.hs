module Main where

import Terminal
import Data.Maybe
import BufferManager
import Graphics.Vty

main :: IO ()
main = mkVty >>= \vty ->
       let wtm = initWTM newBM vty in
       do showWTM wtm
          ev <- getKey wtm
          case ev of
            EvKey (KASCII 'q') [] -> return ()
            EvKey (KASCII ':') _  -> do com <- getCommand wtm -- Just an example to show how to use
                                        showWTM wtm
                                      --  print $ (fromJust com)-- TODO: Get actually doing something
            EvResize nx ny -> showWTM wtm
            _ -> showWTM wtm
          shutdown vty