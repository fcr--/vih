module TerminalInterfaces where

import Terminal
import Control.Monad.Reader
import BufferManager
-- Operaciones para navegar el archivo actual
--
{-
--Getter y setter de posiciones en el buffer (de linea)
getBuffPos :: WTManager -> Int
setBuffPos :: WTManager -> Int -> IO WTManager
-}
--Getter de tamanio de buffer
getBuffSize :: WTManager -> Int

--Getter y setter de linea. Para sustituir toda la linea
getLine :: WTManager -> Int -> String
setLine :: WTManager -> Int -> String -> IO WTManager

--Getter y setters de posiciones en la window.
getXpos :: WTManager -> Int
getYpos :: WTManager -> Int
setXpos :: WTManager -> Int -> IO WTManager
setYpos :: WTManager -> Int -> IO WTManager

--Getter de tamanio en largo (de lineas) del archivo y de largo
--de la linea del buffer
getXsize :: WTManager -> Int
getYsize :: WTManager -> Int

--Para mover la ventana para arriba o para abajo
winUp,winDown :: WTManager -> IO WTManager

openLine :: Bool -> WTManager -> IO WTManager

--Para abrir archivos y asociarlos a un buffer en que estemos parados
--O grabar el buffer en el que estamos parados en el archivo dado
openFile :: WTManager -> String -> IO WTManager
writeFile :: WTManager -> Maybe String -> IO WTManager
--Interfaces file for WTManager (Terminal.hs)

setX xs nx = init xs ++ [nx]
bmOp :: (BManager->Int->a)->WTManager->a
bmOp f wtm = snd.runReader (nav (curwdw wtm) (lo wtm)) (bm wtm)
    where nav (x:xs) lay =
            case lay of
                (Hspan w h lst) -> nav xs (fst $lst!!x)
                (Vspan w h lst) -> nav xs (fst $lst!!x)
                (Window (x,y) z) -> ask >>= \bm -> return $ f bm z
                _ -> undefined
loOp :: (BManager->Int->a)->(Layout,Layout)->WTManager->(WTManager,a)
loOp f wtm g= let localLo = runReader (navIO (curwdw wtm) (lo wtm)) (bm wtm) in (localLo, f bm z)
    where
        navIO (x:xs) lo = 
            case lo of (Hspan w h lst) -> return $ Hspan w h (take x lst ++ [(\(l,s)-> (navIO xs l,s) )] ++ drop (x+1) lst)
                       (Vspan w h lst) -> return $ Vspan w h (take x lst ++ [(\(l,s)-> (navIO xs l,s) )] ++ drop (x+1) lst)
                       win@(Window (x,y) z) -> g win


getBuffSize wtm = getBuffSizeBM (bm wtm)

getLine wtm lnum = bmOp getLineBM wtm 
setLine wtm lnum nstr = return ( loOp setLineBM id wtm ) >>= \cosa-> showWTM.fst cosa >> return cosa

getXpos wtm = last.curwdw wtm
getYpos wtm = bmOp getYposBM wtm
setXpos wtm nx = return ( wtm{curwdw = setX (curwdw wtm) nx} ) >>= \cosa -> showWTM cosa >> return cosa
setYpos wtm ny = return (\(wtman,nBM) -> wtman{bm = nBM}) $ loOp setYposBM id wtm >>= \cosa -> showWTM cosa >> return cosa

getXsize wtm = last $ curwdw wtm
{-
getXpos :: WTManager -> Int
getYpos :: WTManager -> Int
setXpos :: WTManager -> Int -> IO WTManager
setYpos :: WTManager -> Int -> IO WTManager

--Getter de tamanio en largo (de lineas) del archivo y de largo
--de la linea del buffer
getXsize :: WTManager -> Int
getYsize :: WTManager -> Int

--Para mover la ventana para arriba o para abajo
winUp,winDown :: WTManager -> IO WTManager

--Para abrir archivos y asociarlos a un buffer en que estemos parados
--O grabar el buffer en el que estamos parados en el archivo dado
openFile :: WTManager -> IO WTManager
writeFile :: WTManager -> Maybe String -> IO WTManager
--Interfaces file for WTManager (Terminal.hs)
-}

{-
getBuffPos wtm = nav (curwdw wtm) (lo wtm)
    where nav (x:xs) lay =
            case lay of
                (Hspan x y lst) = nav xs (lst!!x)
                (Vspan x y lst) = nav xs (lst!!x)
                (Window (x,y) z) = z
          nav sth lay = undefined
setBuffPos wtm npos = do
                 things <- wtm{lo = navIO (curwdw wtm) (lo wtm)}
                 (DisplayRegion w h) <- (terminal.vty wtm)
                 printTerm things --a implementar en Terminal.hs
                 return things
    where navIO (x:xs) lay =
            case lay of
                (Hspan x y lst) = Hspan x y (take (x-1) lay ++ [(\(lout,s) -> (navIO xs lout,s))] ++ drop x lay)
                (Vspan x y lst) = Vspan x y (take (x-1) lay ++ [(\(lout,s) -> (navIO xs lout,s))] ++ drop x lay)
                (Window (x,y) z) = Window (x,y) z
                xs = xs -}
