module TerminalInterfaces where
import Terminal
import Control.Monad.Reader
import Control.Monad.State
import BufferManager
import Debug.Trace
import Graphics.Vty
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

-- Para obtener datos del buffer local
bmOp :: (BManager->Int->a)->WTManager->a
bmOp f wtm = runReader (nav (curwdw wtm) (lo wtm)) (bm wtm)
    where nav (x:xs) lay = case lay of
                             (Hspan w h lst) -> nav xs (fst $lst!!x)
                             (Vspan w h lst) -> nav xs (fst $lst!!x)
                             (Window (x,y) z) -> ask >>= \bm -> return $ f bm z
                             _ -> undefined
          nav _ (Window _ z) = ask >>= \bm -> return $ f bm z
-- Para ejecutar una funcion que modifica el buffer local y el layout local
loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)
loop _ _ wtm  | trace (show $ curwdw wtm) True = undefined
loOp f g wtm = let (retf, (bmNew,z)) = runState (navIO (curwdw wtm) (lo wtm)) (bm,0) in (wtm{lo = retf},f (bm wtm) z)
    where navIO (x:xs) lo = 
              case lo of (Hspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Hspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                         (Vspan w h lst) -> (\(l,s)-> navIO xs l >>= \a-> return ((a,s)) ) (lst!!x) >>= \obj-> return $ Vspan w h (take x lst ++ [obj] ++ drop (x+1) lst)
                         win@(Window (x,y) z) -> return (g win) >>= \cosa -> modify (\(a,b)->(a,z)) >> return cosa
          navIO _ win@(Window _ z) = return (g win) >>= \cosa -> modify (\(a,b)->(a,z)) >> return cosa

getBuffSize wtm = getBuffSizeBM (bm wtm)

getLine wtm lnum = bmOp ((\a b c -> getLineBM b c a) lnum) wtm 
setLine wtm lnum nstr = return ( loOp setLineBM id wtm ) >>= \(cosa,f) -> let wtm' = cosa {bm = f lnum nstr} in  showWTM wtm'

getXpos wtm = bmOp getXposBM wtm
getYpos wtm = bmOp getYposBM wtm

setXpos wtm nx = return ((\(wtman,nBM) -> wtman{bm = nBM nx }) $ loOp setXposBM id wtm) >>= showWTM
setYpos wtm ny = return ((\(wtman,nBM) -> wtman{bm = nBM ny }) $ loOp setYposBM id wtm) >>= showWTM

getXsize wtm = bmOp getXsizeBM wtm
getYsize wtm = bmOp getYsizeBM wtm 

deleteLine :: WTManager -> IO WTManager
deleteLine wtm = do
			let (wtm',bm')  = loOp deleteLineBM id wtm
			showWTM $ wtm' { bm = bm' }


--TODO: DEFINE STUFF
wsth :: (BManager -> Int -> BManager ) -> WTManager -> WTManager
wsth f wtm = wtm{bm = runReader (navIO (curwdw wtm) (lo wtm)) (bm wtm)}
    where navIO (x:xs) lo = case lo of
                              (Hspan a b lst) -> navIO xs (fst (lst!!x))
                              (Vspan a b lst) -> navIO xs (fst (lst!!x))
                              (Window (x,y) z) -> ask >>= \a -> return (f a z)

winUp wtm = return (wsth winUpBM wtm) >>= showWTM
winDown wtm = return (wsth winDownBM wtm) >>=  showWTM
openLine updown wtm = (\fruta -> return ((\(wt,bx) -> wt{bm = bx}) $ loOp ((\boolvar rest1 rest2 -> openLineBM rest1 rest2 boolvar) fruta) id wtm) >>=  showWTM) updown

-- bmOp :: (BManager->Int->a)->WTManager->a
-- openFile :: WTManager -> String -> IO WTManager
openFile wtm str = do nbm <- bmOp ((\a b c -> openFileBM b c a) str) wtm
                      showWTM (wtm{bm = nbm})

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
                        showWTM (wtm{curwdw = xs})
_loMove :: Layout -> [Int] -> Pos -> ([Int],Bool)
_loMove lo (x:xs) pos = case lo of
            (Window _ _) -> (xs,False)
            (Hspan a b lst) -> let ((cab:cola),stuff) = _loMove (fst (lst!!x)) xs pos in if stuff then (x:cab:cola,True) else if not (horiz pos) then (x:xs,False) else (x:repeat 0,True)
            (Vspan a b lst) -> let ((cab:cola),stuff) = _loMove (fst (lst!!x)) xs pos in if stuff then (x:cab:cola,True) else if not (verti pos) then (x:xs,False) else (x:repeat 0,True)
-- writeFile :: WTManager -> Maybe String -> IO WTManager
writeFile wtm mbstr = (\(wtm,bas) -> bas mbstr >>= \c-> return (wtm{bm = c})) (loOp writeFileBM id wtm)

--loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)

closeWin :: WTManager -> IO WTManager
closeWin wtm = do
        let b = (\(wt,bf) -> wt{bm = bf} ) $ loOp deleteBuffer id wtm
        (DisplayRegion w h) <- display_bounds $ terminal $ vty b
        a <- return $ resizeLayout (fromIntegral w) (fromIntegral h) (b{lo = fst(navLayout (lo b) (curwdw b))})
        showWTM a >>= \x -> return x {curwdw = newcurwdw x}
    where 
        navLayout lt (x:xs) = if length xs > 3 then 
                (case lt of 
                    (Hspan w h lst) -> let (nlo,horiz) = navLayout (fst(lst!!x)) xs in if horiz then mergeLayout horiz nlo w h lst x else (Hspan w h $ (take x lst) ++ [(nlo,0)] ++ (drop (x+1) lst),True)
                    (Vspan w h lst) -> let (nlo,horiz) = navLayout (fst(lst!!x)) xs in if not horiz then mergeLayout horiz nlo w h lst x else (Vspan w h $ (take x lst) ++ [(nlo,0)] ++ (drop (x+1) lst),False)
                    _ -> error "closeWin")
            else
                (case lt of
                    (Hspan w h lst) -> (Hspan w h (take x lst ++ drop (x+1) lst),True)
                    (Vspan w h lst) -> (Vspan w h (take x lst ++ drop (x+1) lst),False))
        mergeLayout horiz nlo w h lst x = case horiz of
            True -> case nlo of
                (Hspan _ _ botlst) -> (Hspan 0 0 (take x lst ++ botlst ++ drop (x+1) lst),True)
                _ -> error "true closeWin"
            False -> case nlo of
                (Vspan _ _ botlst) -> (Vspan 0 0 (take x lst ++ botlst ++ drop (x+1) lst),False)
newcurwdw :: WTManager -> [Int]
newcurwdw wtm = getNewWin (lo wtm)
    where
        getNewWin :: Layout -> [Int]
        getNewWin lo = case lo of
            (Hspan _ _ lst) -> (:) 0 $ getNewWin $ (fst.head) lst
            (Vspan _ _ lst) -> (:) 0 $ getNewWin $ (fst.head) lst
            (Window _ _) -> [0,0]
