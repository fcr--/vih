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
-- initWTM >>= \wtm -> winMove wtm U >>= \wtm2 -> shutdown (vty wtm2) >> return (curb wtm2)
winMove :: WTManager -> Pos -> IO WTManager
winMove wtm pos = let (xs,b) = _loMove (lo wtm) (curwdw wtm ++ repeat 0 ) pos in
                    do
                        case b of
				True -> let cr = fix (lo wtm) xs in showWTM (wtm{curwdw = cr, curb = getBuff cr (lo wtm)})
                                False -> showWTM wtm
	where
		fix (Window _ _) xs = []
		fix (Hspan _ _ lst ) (x:xs) = x : fix (fst $ lst !! x) xs
		fix (Vspan _ _ lst ) (x:xs) = x : fix (fst $ lst !! x) xs

_loMove :: Layout -> [Int] -> Pos -> ([Int],Bool)
_loMove lo (x: xs) pos = case lo of
                            Hspan w h lst -> let (a, b) = _loMove (fst (lst!!x)) xs pos in let l = length lst - 1 in
					     case b of
						True -> (x : a, b)
                                                _ -> if x == 0 && pos == L || x == l && pos == R || verti pos then -- failure
							([], b)
						     else  ( (x + toInt pos) : (repeat 0) , True)
                            Vspan w h lst -> let (a, b) = _loMove (fst (lst!!x)) xs pos in let l = length lst - 1 in
					     case b of
						True -> (x : a, b)
                                                _ -> if x == 0 && pos == U || x == l && pos == D || horiz pos then -- failure
							([], b)
						     else  ( (x + toInt pos) :  (repeat 0) , True)
                            Window _ _    -> ([],False)

_loMove lo [] pos	=	([],False)


clamp z b = max 0 $ min b z

toInt :: Pos -> Int
toInt L = -1
toInt R = 1
toInt D = 1
toInt U = -1

-- _loMove (Window _ _) xs _ = (xs,False)
-- writeFile :: WTManager -> Maybe String -> IO WTManager
writeFile wtm mbstr = do
			wtm <- (\(wtm,bas) -> bas mbstr >>= \c-> return (wtm{bm = c})) (loOp writeFileBM id wtm)
                        showWTM wtm

--loOp :: (BManager -> Int -> a) -> (Layout -> Layout) -> WTManager -> (WTManager,a)

closeWin :: WTManager -> IO WTManager
closeWin wtm = do
        let b = (\(wt,bf) -> wt{bm = bf} ) $ loOp deleteBuffer id wtm
        (DisplayRegion w h) <- display_bounds $ terminal $ vty b
        a <- return $ resizeLayout (fromIntegral w) (fromIntegral h) (b{lo = fst(navLayout (lo b) (curwdw b))})
        showWTM a >>= \x -> let cr = newcurwdw x in return x {curwdw = cr, curb = getBuff cr (lo x)}
    where 
        navLayout lt (x:xs) = (\(x,y) -> (clamp x, y) ) $
                case lt of 
                    (Hspan w h lst) -> let (nlo,emp) = navLayout (fst(lst!!x)) xs in if emp then (Hspan w h $ (take x lst) ++ (drop (x+1) lst),len1 lst) else merge x lt nlo
                    (Vspan w h lst) -> let (nlo,emp) = navLayout (fst(lst!!x)) xs in if emp then (Vspan w h $ (take x lst) ++ (drop (x+1) lst),len1 lst) else merge x lt nlo
                    _ -> (undefined,True)
	navLayout _ _ = (undefined,True)
	len1 [] = False
	len1 (x:y:ys) = False
	len1 _ = True
	clamp (Hspan w h [el]) = fst el
	clamp (Vspan w h [el]) = fst el
	clamp x = x
	merge x (Hspan w h lst) (Hspan _ _ rs) = (Hspan w h $ (take x lst) ++ rs ++ (drop (x+1) lst),False)
	merge x (Hspan w h lst) nlo = (Hspan w h $ (take x lst) ++ [(nlo,undefined)] ++ (drop (x+1) lst),False)
	merge x (Vspan w h lst) (Vspan _ _ rs) = (Vspan w h $ (take x lst) ++ rs ++ (drop (x+1) lst),False)
	merge x (Vspan w h lst) nlo = (Vspan w h $ (take x lst) ++ [(nlo,undefined)] ++ (drop (x+1) lst),False)

newcurwdw :: WTManager -> [Int]
newcurwdw wtm = getNewWin (lo wtm)
    where
        getNewWin :: Layout -> [Int]
        getNewWin lo = case lo of
            (Hspan _ _ lst) -> (:) 0 $ getNewWin $ (fst.head) lst
            (Vspan _ _ lst) -> (:) 0 $ getNewWin $ (fst.head) lst
            (Window _ _) -> []
