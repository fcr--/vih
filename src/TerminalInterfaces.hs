<<<<<<< HEAD:src/TerminalInterfaces.hs
module TerminalInterfaces where

=======
module Main where
>>>>>>> 3b8138dfc519ecc8689f9421b71214995866b3a3:src/TerminalInterfaces.hs
import Terminal
import Control.Monad.Reader
import Control.Monad.State
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
-- Para obtener datos del buffer local
bmOp :: (BManager->Int->a)->WTManager->a
bmOp f wtm = runReader (nav (curwdw wtm) (lo wtm)) (bm wtm)
    where nav (x:xs) lay =
            case lay of
                (Hspan w h lst) -> nav xs (fst $lst!!x)
                (Vspan w h lst) -> nav xs (fst $lst!!x)
                (Window (x,y) z) -> ask >>= \bm -> return $ f bm z
                _ -> undefined
-- Para ejecutar una funcion que modifica el buffer local y el layout local
loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)
loOp f g wtm = let (retf, (bmNew,z)) = runState (navIO (curwdw wtm) (lo wtm)) (bm,0) in (wtm{lo = retf},f (bm wtm) z)
    where
        navIO (x:xs) lo = 
            case lo of (Hspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Hspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                       (Vspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Vspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                       win@(Window (x,y) z) -> return (g win) >>= \cosa -> modify (\(a,b)->(a,z)) >> return cosa

getBuffSize wtm = getBuffSizeBM (bm wtm)

getLine wtm lnum = bmOp ((\a b c -> getLineBM b c a) lnum) wtm 
setLine wtm lnum nstr = return ( loOp setLineBM id wtm ) >>= \(cosa,f) -> showWTM cosa >> return cosa

getXpos wtm = bmOp getCursor wtm
getYpos wtm = bmOp getYposBM wtm
--CUALQUIERA
{-
setXpos wtm nx = return ( wtm{curwdw = setX (curwdw wtm) nx} ) >>= \cosa -> showWTM cosa >> return cosa
-}

setXpos wtm nx = return ((\(wtman,nBM) -> wtman{bm = nBM nx }) $ loOp setCursor id wtm) >>= \cosa -> showWTM cosa >> return cosa
setYpos wtm ny = return ((\(wtman,nBM) -> wtman{bm = nBM ny }) $ loOp setYposBM id wtm) >>= \cosa -> showWTM cosa >> return cosa

getXsize wtm = bmOp getCursor wtm
getYsize wtm = bmOp getYsizeBM wtm 


--TODO: DEFINE STUFF
winUp = undefined
winDown = undefined
openLine = undefined
openFile = undefined
writeFile = undefined
{-
getXpos :: WTManager -> Int
getYpos :: WTManager -> Int
setXpos :: WTManager -> Int -> IO WTManager
setYpos :: WTManager -> Int -> IO WTManager

--Getter de tamanio en largo (de lineas) del archivo y de largo
--de la linea del buffer getXsize :: WTManager -> Int
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
main :: IO ()
main = undefined  
-- vi: et sw=4
