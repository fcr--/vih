module Main where

import Terminal
import TerminalInterfaces
import Data.Maybe
import BufferManager
import Graphics.Vty
{-
main :: IO ()
main = do wtm <- initWTM
          showWTM wtm
          ev <- getKey wtm
          case ev of
            EvKey (KASCII 'q') [] -> return ()
            EvKey (KASCII ':') _  -> do com <- getCommand wtm -- Just an example to show how to use
                                        showWTM wtm
                                      --  print $ (fromJust com)-- TODO: Get actually doing something
            EvResize nx ny -> showWTM wtm
            _ -> showWTM wtm
          shutdown (vty wtm)
-}
main :: IO ()
main = initWTM >>= mainloop

mainloop wtm = do
    wtx <- showWTM wtm
    (ev,wt) <- getKey wtx
    case ev of
        EvKey _ _ -> shutdown (vty wt)
        _ -> mainloop wt
    
    
