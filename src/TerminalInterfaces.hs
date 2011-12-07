import Terminal.hs

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
getLine :: WTManager -> Int -> IO (WTManager, String)
setLine :: WTManager -> Int -> String -> IO (WTManager, String)

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

--Para abrir archivos y asociarlos a un buffer en que estemos parados
--O grabar el buffer en el que estamos parados en el archivo dado
openFile :: WTManager -> IO WTManager
writeFile :: WTManager -> Maybe String -> IO WTManager
--Interfaces file for WTManager (Terminal.hs)

getBuffSize wtm = runReader (nav (curwdw wtm) (lo wtm)) (bm wtm)
    where nav (x:xs) lay = 
            case lay of
                (Hspan w h lst) -> nav xs (lst!!x)
                (Vspan w h lst) -> nav xs (lst!!x)
                (Window (x,y) z) -> ask >>= \bm -> return $ getBuffSizeBM z bm
                _ -> undefined

getLine wtm lnum 
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
