module TerminalInterfaces where
import Terminal
import Control.Monad.Reader
import Control.Monad.State
import BufferManager
import Prelude hiding (readFile)
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
    where nav (x:xs) lay = case lay of
                             (Hspan w h lst) -> nav xs (fst $lst!!x)
                             (Vspan w h lst) -> nav xs (fst $lst!!x)
                             (Window (x,y) z) -> ask >>= \bm -> return $ f bm z
                             _ -> undefined
-- Para ejecutar una funcion que modifica el buffer local y el layout local
loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)
loOp f g wtm = let (retf, (bmNew,z)) = runState (navIO (curwdw wtm) (lo wtm)) (bm,0) in (wtm{lo = retf},f (bm wtm) z)
    where navIO (x:xs) lo = 
              case lo of (Hspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Hspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                         (Vspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Vspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                         win@(Window (x,y) z) -> return (g win) >>= \cosa -> modify (\(a,b)->(a,z)) >> return cosa

getBuffSize wtm = getBuffSizeBM (bm wtm)

getLine wtm lnum = bmOp ((\a b c -> getLineBM b c a) lnum) wtm 
setLine wtm lnum nstr = return ( loOp setLineBM id wtm ) >>= \(cosa,f) -> showWTM cosa >> return cosa

getXpos wtm = bmOp getXposBM wtm
getYpos wtm = bmOp getYposBM wtm

setXpos wtm nx = return ((\(wtman,nBM) -> wtman{bm = nBM nx }) $ loOp setXposBM id wtm) >>= \cosa -> showWTM cosa >> return cosa
setYpos wtm ny = return ((\(wtman,nBM) -> wtman{bm = nBM ny }) $ loOp setYposBM id wtm) >>= \cosa -> showWTM cosa >> return cosa

getXsize wtm = bmOp getXsizeBM wtm
getYsize wtm = bmOp getYsizeBM wtm 


--TODO: DEFINE STUFF
wsth :: (BManager -> Int -> Int -> Int -> BManager ) -> WTManager -> WTManager
wsth f wtm = wtm{bm = runReader (navIO (curwdw wtm) (lo wtm)) (bm wtm)}
    where navIO (x:xs) lo = case lo of
                              (Hspan a b lst) -> navIO xs (fst (lst!!x))
                              (Vspan a b lst) -> navIO xs (fst (lst!!x))
                              (Window (x,y) z) -> ask >>= \a -> return (f a z x y)

winUp wtm = return (wsth winUpBM wtm) >>= \wtmn -> showWTM wtmn >> return wtmn
winDown wtm = return (wsth winDownBM wtm) >>= \wtmn -> showWTM wtmn >> return wtmn
openLine updown wtm = (\fruta -> return ((\(wt,bx) -> wt{bm = bx}) $ loOp ((\boolvar rest1 rest2 -> openLineBM rest1 rest2 boolvar) fruta) id wtm) >>= \cosa -> showWTM cosa >> return cosa) updown

-- bmOp :: (BManager->Int->a)->WTManager->a
-- openFile :: WTManager -> String -> IO WTManager
openFile wtm str = do nbm <- bmOp ((\a b c -> openFileBM b c a) str) wtm
                      wtmnew <- return $ wtm{bm = nbm}
                      showWTM wtmnew
                      return wtmnew
    where openFileBM = error "FIXME!!!"

-- To move left/up/down/right
data Pos = L | U | D | R deriving(Eq)
horiz :: Pos -> Bool
horiz x = case x of
    L -> True
    R -> True
    _ -> False
verti :: Pos -> Bool
verti = not.horiz
navLev :: Pos -> Bool
navLev x = case x of
    L -> True
    U -> True
    _ -> False
-- navigate position Pos with starting position Int in an array of size Int (2) and 
navigatePos :: Pos -> Int -> Int -> Int
navigatePos pos toNav lenPlace | toNav == 0 && navLev pos == True = 0
                               | toNav == (lenPlace-1) && not (navLev pos) == False = lenPlace -1
                               | otherwise = (if navLev pos then -1 else 1) + toNav

winMove :: WTManager -> Pos -> IO WTManager
winMove wtm pos = let (xs,_) = _loMove (lo wtm) (curwdw wtm) pos in
                    do
                        newWTM <- return (wtm{curwdw = xs})
                        showWTM newWTM
                        return newWTM
_loMove :: Layout -> [Int] -> Pos -> ([Int],Bool)
_loMove lo (x:xs) pos = case lo of
            (Window _ _) -> (xs,False)
            (Hspan a b lst) -> let ((cab:cola),stuff) = _loMove (fst (lst!!x)) xs pos in if stuff then (x:cab:cola,True) else if not (horiz pos) then (x:xs,False) else (x:repeat 0,True)
            (Vspan a b lst) -> let ((cab:cola),stuff) = _loMove (fst (lst!!x)) xs pos in if stuff then (x:cab:cola,True) else if not (verti pos) then (x:xs,False) else (x:repeat 0,True)
-- writeFile :: WTManager -> Maybe String -> IO WTManager
writeFile wtm mbstr = (\(wtm,bas) -> bas mbstr >>= \c-> return (wtm{bm = c})) (loOp writeFileBM id wtm)

--loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)

-- vi: et sw=4
