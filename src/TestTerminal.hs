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
main = initWTM >>= \wtm -> openFileBM (bm wtm) 0 "TestTerminal.hs" >>= \bm2 ->  mainloop $ wtm { bm = bm2 }

mainloop wtm = do
    wtx <- showWTM wtm
    (ev,wt) <- getKey wtx
    case ev of
        EvKey (KASCII 'q') _ -> shutdown (vty wt)
	EvKey (KUp) _ -> let b = bm wt in  mainloop $ wt { bm =   setYposBM b 0 (max 0 $ (getYposBM b 0) - 1) } 
	EvKey (KDown) _ -> let b = bm wt in  mainloop $ wt { bm =   setYposBM b 0 ((getYposBM b 0) + 1) } 
	EvKey (KASCII 'o') _ -> let b = bm wt in  mainloop $ wt { bm =  openLineBM b 0 True }
        EvKey (KASCII c) _ -> let b = bm wt in  mainloop $ wt { bm =  setXposBM ( setLineBM b 0 (getYposBM b 0) ( (getLineBM b 0 (getYposBM b 0)) ++ [c]) ) 0 ( (getXposBM b 0 ) + 1) }
        _ -> mainloop wt
    
    
