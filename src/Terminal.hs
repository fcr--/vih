
{-#OPTIONS -XMultiParamTypeClasses #-}

module Terminal where
import BufferManager(BManager,newBM,newBuffer,printWinBM,openFileBM)
import Debug.Trace
import Data.Map(Map,(!),singleton,keys,insert)
import Graphics.Vty
import Data.Word
import Control.Monad.State
import Control.Monad.Reader
import Data.List(unfoldr)
------ Data definitions ------


--Data holder of a windows tiling manager
--Except the very last line, the whole terminal is described by the layout (except maybe overlays)
data WTManager = WTMa {lo :: Layout -- current layout
                      ,curwdw :: [Int] -- current window being used
                      ,wtmH :: Int -- height in characters of window
                      ,wtmW :: Int -- width in characters of window
                      ,bm :: BManager
                      ,stLine :: String
                      ,vty :: Vty
                      ,curb :: Int
                      }

 -- The way to initialize the window manager at program startup
 -- TODO: change wsizes, Window initializer, possibly add a startup
 -- size variable
initWTM :: IO WTManager
initWTM = do
    v <- mkVty
    --show_cursor $ terminal v
    (DisplayRegion w h) <- display_bounds (terminal v)
    let wtm = resizeLayout (fromIntegral w) (fromIntegral h) $ WTMa {lo = Window (undefined, undefined) 0, curwdw = [], wtmH = 0, wtmW = 0, bm = newBM, stLine = "Welcome to VIH.", vty = v, curb = 0}
    --wtm'  <- newWin True wtm --  TODO : DE MUESTRA ESTO
    --wtm'' <- newWin False wtm'
    --wtm3  <- newWin False wtm''
    --wtm4  <- newWin False wtm3
    return $ resizeLayout (fromIntegral w) (fromIntegral h) wtm -- $ wtm4
    
--Data type that wraps the command line attributes
data CommandLine = CM {comm :: String, pos :: Int}

--Data representing current tiling. example:
--
-- Hspan 14 9 [Vspan 5 5 [Window 3 3 b1,Window 3 2 b2],Window 8 6 b3]
--
-- 12345678901234
-- **************
-- *1  *1       *
-- * 2 * 2      *
-- *  3*  3     *
-- *****   4    *
-- *1  *    5   *
-- * 23*     678*
-- **************
-- *commandline *

-- Layout explanation:
-- Cons X Y Sth:
--      X = horizontal span of the component
--      Y = vertical span of the component
--      Sth = content of the component
-- (H)Vspan = X Y [(Cp,Int)] = v/h span of component, plus spans of subcomponents
--
data Layout = Vspan Int Int [(Layout,Int)] --Vertical span with sizes
             |Hspan Int Int [(Layout,Int)] --Horizontal span with sizes
             |Window {size::(Int,Int), buffNum::Int} -- Window with buffer number
             |NoWin{size :: (Int,Int)}
             deriving(Show)

bgcolor = def_attr `with_fore_color` red
dum = def_attr `with_style` reverse_video
cbarras = def_attr `with_style` reverse_video `with_fore_color` red

--Function to resize the current layout to fit the new window size
resizeLayout::Int->Int->WTManager->WTManager
resizeLayout w h wtm = wtm{lo = (resizeLo w (h-2)) (lo wtm),wtmH = h-2, wtmW = w}

resizeLo :: Int -> Int -> Layout -> Layout
resizeLo w h lo = case lo of
                    (Vspan px py xs) -> Vspan w h $(funcV w h xs)
                    (Hspan px py xs) -> Hspan w h $(funcH w h xs)
                    Window _ bn -> Window (w,h) bn
                    NoWin _ -> NoWin (w,h)
    where
        funcV w h lst = let (d,m) = divMod (h-(length lst)+1) (length lst) in map (\(l,lst) -> (resizeLo w (d+1) l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo w d l,d)) (drop m lst)
        funcH w h lst = let (d,m) = divMod (w-(length lst)+1) (length lst) in map (\(l,lst) -> (resizeLo (d+1) h l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo d h l,d)) (drop m lst)

--Get the terminal's height
getH :: DisplayRegion -> Word
getH (DisplayRegion w h) = h

--Get the terminal's weight
getW :: DisplayRegion -> Word
getW (DisplayRegion w h) = w


--Function to print the vty contained in the wtm
showWTM :: WTManager -> IO WTManager
showWTM wtm = do
  (DisplayRegion w h) <- display_bounds $ terminal (vty wtm)
  (nwtm,im) <- armarIm (fromIntegral w) (fromIntegral h) wtm 
  update (vty nwtm) (pic_for_image im)
  --show_cursor ( terminal $ vty nwtm)
  return nwtm{wtmW =fromIntegral w, wtmH = fromIntegral (h - 2)}


--Function to get the next event from the Vty
getKey :: WTManager -> IO (Event,WTManager)
getKey wtm = do nEv <- next_event (vty wtm)
                case nEv of
                  EvResize nx ny -> do
                        let wt = resizeLayout nx ny wtm
                        showWTM wt
                        getKey wt
                  _ -> return (nEv,wtm)

--Function to get a command from the command line
getCommand :: String -> WTManager -> IO (WTManager,Maybe String)
getCommand prompt wtm = let vty' = (vty wtm) in
                        do bnds <- display_bounds (terminal vty')
                           getCommand' line wtm (getW bnds) (getH bnds)
    where line = CM {comm = prompt, pos = 1}

--Function that asks continully for the next character of the command until it ends and return the string that composes it
getCommand' :: CommandLine -> WTManager -> Word -> Word -> IO (WTManager,Maybe String)
getCommand' cl wtm w h = do
    updateVtyCommand wtm (comm cl) w h
    set_cursor_pos (terminal $ vty wtm) (toEnum $ pos cl) (h-1)
    show_cursor (terminal $ vty wtm)
    (nEv,wtm') <- getKey wtm
    hide_cursor (terminal $ vty wtm)
    case nEv of
        EvKey (KASCII c) _ -> let newCL = addCharComm cl c
                              in do updateVtyCommand wtm' (comm newCL) w h
                                    getCommand' newCL wtm' w h
        EvKey (KEnter) _ -> do updateVtyCommand wtm "" w h
                               return (wtm', Just $ comm cl)
        EvKey (KBS) _ -> let newCL = suprComm cl
                         in do updateVtyCommand wtm' (comm newCL) w h
                               getCommand' newCL wtm' w h
        EvKey (KDel) _ -> let newCL = delComm cl
                          in do updateVtyCommand wtm' (comm newCL) w h
                                getCommand' newCL wtm' w h
        EvKey (KLeft) _ -> let cur = pos cl
                           in if cur <= 1
                              then getCommand' cl wtm' w h
                              else getCommand' (cl {pos = cur - 1}) wtm' w h
        EvKey (KRight) _ -> let cur = pos cl
                            in if cur == length (comm cl)
                               then getCommand' cl wtm' w h
                               else getCommand' (cl {pos = cur + 1}) wtm' w h 
        EvKey (KEsc) _ -> do nwtm <- showWTM wtm'
                             return (nwtm,Nothing)
        EvResize nx ny -> do let nwt = resizeLayout nx ny wtm'
                             showWTM (nwt)
                             getCommand' cl nwt w h  
        _ -> getCommand' cl wtm w h

--Function that updates the Vty on screen with the given String placed on the command line
updateVtyCommand :: WTManager -> String -> Word -> Word -> IO ()
updateVtyCommand wtm comm w h = do
    update (vty wtm) (pic_for_image $ armarCommand comm (fromIntegral w) (fromIntegral h) wtm)

--Function that adds a character to the command line at the current position (Cursor)
addCharComm :: CommandLine -> Char -> CommandLine
addCharComm cl c = let (a,b) = splitAt (pos cl) (comm cl)
                       i = pos cl
                   in cl {comm = a++[c]++b, pos = i+1}

--Function that suprimes a character at the current position in the command line (Backward delete)
suprComm :: CommandLine -> CommandLine
suprComm cl = let (a,b) = splitAt (pos cl) (comm cl)
                  i = pos cl
              in if i <= 1 then cl else cl {comm = (init a)++b, pos = i -1}

--Function that deletes a charcter at the current position in the command line (Forward delete)
delComm :: CommandLine -> CommandLine
delComm cl = let (a,b) = splitAt (pos cl) (comm cl)
             in cl {comm = a++(drop 1 b)}

--Function that paints an empty window
armarIm :: Word -> Word -> WTManager -> IO (WTManager,Image)
armarIm w h wtm = return (printWTM wtm) >>= \(nwtm,im)->return (nwtm, ab im) 
    where 
    ab im = if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
	     else im <-> bordeInferior w <-> string def_attr (stLine wtm)
        
--Function that sets the string holding in the status line
setSt stl wtm = wtm {stLine = stl}

--Function that paints the command line with the accumulative string given
armarCommand s w h wtm = if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
                         else im <-> bordeInferior w <-> string def_attr (take w $ s ++ repeat ' ')
    where
    im = crop (toEnum w, toEnum h-3) $ snd $ printWTM wtm
                          
--bordeSuperior w = char_fill dum '-' w 1 -- VI no llevar borde superior!
bordeInferior w = char_fill dum '-' w 1
-- TODO: mirar...
printNoWin w h |(w<1) || (h<1) = empty_image
               | otherwise = (string def_attr s <|> char_fill def_attr '#' (w-length s) 1) <-> char_fill dum '-' w (h-2) <-> bordeInferior w
                    where
                        s = take w "Ventana vacia. Utilice el comando TODO :agregar comando"
printWin w h = error "printWin"

printWTM :: WTManager -> (WTManager,Image)
printWTM wtm = (\(a,b) -> (b,a)) $ runState (printLayout (lo wtm)) (wtm)

printControl lOut = string def_attr $ show lOut
printLayout :: Layout -> State WTManager Image
printLayout lOut = case lOut of
                        NoWin (x,y) -> return $ printNoWin x y
                        (Window (x,y) num) -> gets (\wm-> printWinBM (bm wm) num (num == curb wm) (x,y)) >>= \(im,nbm) ->  modify (\wtm -> wtm{bm = nbm}) >> return im
                        (Hspan x y xsL) -> mapM (printLayout.fst) xsL >>= return . ( foldr1 (\a b -> a <|> barraVert y <|> b ) )
                        (Vspan x y xsL) -> mapM (printLayout.fst) xsL >>= return . ( foldr1 (\a b -> a <-> barraHoriz x <-> b) )
    where barraVert y = char_fill cbarras '|' 1 y
          barraHoriz x = char_fill cbarras '-' x 1

--Imprimir un buffer en una ventana

-- TODO : CORREGIR EL TEMA DE LA VENTANA EN USO, curwdw es el CAMINO a la ventana en uso.
--Abrir ventana nueva
newWin :: Bool {- horizontal? -} -> WTManager -> IO WTManager
newWin horiz wtm' = do
                (newbm,bnum) <- newBuffer (bm wtm') Nothing
                let wtm = wtm' { bm = newbm }
                (DisplayRegion w h) <- display_bounds (terminal $vty wtm)
                let lWTM = resizeLayout (fromIntegral w) (fromIntegral h) (splitX horiz bnum wtm)
                showWTM lWTM

--Hacer split Horizontal con param = True, vertical con param = False
-- Int = buffer number
splitX :: Bool -> Int -> WTManager -> WTManager
splitX param bn wtm = f $ case changed cw spl of
                        True -> wtm{ lo = spl, curwdw = (\xs -> xs ++ [1]) cw}
                        _ -> wtm {lo = spl, curwdw = (\xs -> init xs ++ [1 + last xs]) cw } 
    where
            cw = curwdw wtm
            spl = splitLoX param bn (lo wtm) cw 
            f wtm = wtm {curb = (getBuff ( curwdw wtm ) (lo wtm )) }

getBuff :: [Int] -> Layout -> Int
getBuff (x:xs) (Vspan _ _ lst)  = getBuff xs ( fst $ lst !! x )
getBuff (x:xs) (Hspan _ _ lst)  = getBuff xs ( fst $ lst !! x )
getBuff _ (Window _ buff)       = buff

changed :: [Int] -> Layout -> Bool
changed (x:xs) (Vspan _ _ lst)  = changed xs ( fst $ lst !! x )
changed (x:xs) (Hspan _ _ lst)  = changed xs ( fst $ lst !! x )
changed _ (Hspan _ _ _)         = True
changed _ (Vspan _ _ _)         = True
changed _ _ = False

splitLoX :: Bool -> Int -> Layout -> [Int] -> Layout
splitLoX param bn l (x: xs@(y':ys)) = case l of
                (Vspan w h lst) -> Vspan w h ((take x) lst ++ [(\(lay,height) -> (splitLoX param bn lay xs,height)) (lst!!x)] ++ drop (x+1) lst)
                (Hspan w h lst) -> Hspan w h ((take x) lst ++ [(\(lay,width) -> (splitLoX param bn lay xs,width)) (lst!!x)] ++ drop (x+1) lst)
                (Window (x,y) z) -> splitLoX param bn l [x] -- Window (x,y)
                (NoWin (x,y)) -> error "splitLoX : NoWin (x,y)" -- NoWin (x,y)
splitLoX param bn l [x] |param = case l of
                                (Vspan w h lst) -> resizeLo w h $  Vspan w h (take x lst ++ [(splitSpan param bn (lst!!x) w)] ++ (drop (x+1) lst))
                                (Hspan w h lst) -> resizeLo w h $  Hspan w h (map (\(l,s) -> (resizeLo w h l,s)) (take (x+1) lst ++ [(Window (undefined,undefined) bn, undefined)] ++  drop (x+1) lst))
                                (Window (w,h) b)-> resizeLo w h $ Hspan w h [(Window (w,h) b,undefined),(Window (w,h) bn,undefined)]
                     |not param = case l of
                                (Vspan w h lst) -> resizeLo w h $ Vspan w h (map (\(l,s) -> (resizeLo w h l,h)) (take (x+1) lst ++ [(Window (w,h) bn,undefined)] ++ drop (x+1) lst))
                                (Hspan w h lst) -> resizeLo w h $ Hspan w h (take x lst ++ [(splitSpan param bn (lst!!x) h)] ++ (drop (x+1) lst))
                                (Window (w,h) b) -> resizeLo w h $ Vspan w h [(Window (w,h) b,undefined),(Window (w,h) bn,undefined)]
    where splitSpan  param bn (lo,s) t |param = (resizeLo t s (Hspan t s [(lo,1),(Window (undefined,undefined) bn, undefined)]),t)
                                       |not param = (resizeLo s t (Vspan s t [(lo,1),(Window (undefined,undefined) bn, undefined)]),t)

splitLoX param bn (Window (w,h) b) _  | param     = resizeLo w h $ Hspan w h [(Window (w,h) b,undefined),(Window (w,h) bn,undefined)]
splitLoX param bn (Window (w,h) b) _  | otherwise =  resizeLo w h $ Vspan w h [(Window (w,h) b,undefined),(Window (w,h) bn,undefined)]

-- vi: et sw=4
