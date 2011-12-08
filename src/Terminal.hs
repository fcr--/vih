
{-#OPTIONS -XMultiParamTypeClasses #-}

module Terminal where
import BufferManager(BManager,newBM,newBuffer)
import Control.Concurrent.STM
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
                      }

 -- The way to initialize the window manager at program startup
 -- TODO: change wsizes, Window initializer, possibly add a startup
 -- size variable
initWTM :: IO WTManager
initWTM = do
    v <- mkVty
    show_cursor $ terminal v
    (DisplayRegion w h) <- display_bounds (terminal v)
    let wtm = WTMa {lo = Window (undefined, undefined) 0, curwdw = [0], wtmH = 0, wtmW = 0, bm = newBM, stLine = "", vty = v}
    return $ resizeLayout (fromIntegral w) (fromIntegral h) wtm
    
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
resizeLayout h w wtm = wtm{lo = (resizeLo (h-2) w) (lo wtm),wtmH = h, wtmW = w}

resizeLo :: Int -> Int -> Layout -> Layout
resizeLo w h lo = case lo of
                    (Vspan px py xs) -> Vspan w h $(funcV w h xs)
                    (Hspan px py xs) -> Hspan w h $(funcH w h xs)
                    Window _ bn -> Window (w,h) bn
                    NoWin _ -> NoWin (w,h)
    where
        funcV w h lst = let (d,m) = divMod h (length lst) in map (\(l,lst) -> (resizeLo w (d+1) l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo w d l,d)) (drop m lst)
        funcH w h lst = let (d,m) = divMod w (length lst) in map (\(l,lst) -> (resizeLo (d+1) h l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo d h l,d)) (drop m lst)

--Get the terminal's height
getH :: DisplayRegion -> Word
getH (DisplayRegion w h) = h

--Get the terminal's weight
getW :: DisplayRegion -> Word
getW (DisplayRegion w h) = w

--Function to print the vty contained in the wtm
showWTM :: WTManager -> IO ()
showWTM wtm = do
  update (vty wtm) (pic_for_image $ armarIm (fromIntegral (wtmW wtm)) (fromIntegral (wtmH wtm)) wtm)
  show_cursor $ terminal $ vty wtm

--Function to get the next event from the Vty
getKey :: WTManager -> IO Event
getKey wtm = do nEv <- next_event (vty wtm)
                case nEv of
                  EvResize nx ny -> do showWTM (resizeLayout nx ny wtm)
                                       getKey wtm
                  _ -> return nEv

--Function to get a command from the command line
getCommand :: WTManager -> IO (Maybe String)
getCommand wtm = let vty' = (vty wtm) in
                 do bnds <- display_bounds (terminal vty')
                    getCommand' line wtm (getW bnds) (getW bnds)
    where line = CM {comm = ":", pos = 1}

--Function that asks continully for the next character of the command until it ends and return the string that composes it
getCommand' :: CommandLine -> WTManager -> Word -> Word -> IO (Maybe String)
getCommand' cl wtm w h= do
    set_cursor_pos (terminal $ vty wtm) (toEnum $ pos cl) (h-1)
    show_cursor (terminal $ vty wtm)
    nEv <- next_event (vty wtm)
    case nEv of
                             EvKey (KASCII c) _ -> let newCL = addCharComm cl c
                                                   in do updateVtyCommand wtm (comm newCL) w h
                                                         getCommand' newCL wtm w h
                             EvKey (KEnter) _ -> do updateVtyCommand wtm "Ejecutando" w h
                                                    return (Just $ comm cl)
                             EvKey (KBS) _ -> let newCL = suprComm cl
                                              in do updateVtyCommand wtm (comm newCL) w h
                                                    getCommand' newCL wtm w h
                             EvKey (KDel) _ -> let newCL = delComm cl
                                               in do updateVtyCommand wtm (comm newCL) w h
                                                     getCommand' newCL wtm w h
                             EvKey (KLeft) _ -> let cur = pos cl
                                                in if cur <= 1
                                                   then getCommand' cl wtm w h
                                                   else getCommand' (cl {pos = cur - 1}) wtm w h
                             EvKey (KRight) _ -> let cur = pos cl
                                                 in if cur == length (comm cl)
                                                    then getCommand' cl wtm w h
                                                    else getCommand' (cl {pos = cur + 1}) wtm w h 
                             EvKey (KEsc) _ -> do showWTM wtm
                                                  return Nothing
                             EvResize nx ny -> do showWTM (resizeLayout nx ny wtm)
                                                  getCommand' cl wtm w h
                             _ -> getCommand' cl wtm w h

--Function that updates the Vty on screen with the given String placed on the command line
updateVtyCommand :: WTManager -> String -> Word -> Word -> IO ()
updateVtyCommand wtm comm w h = update (vty wtm) (pic_for_image $ armarCommand comm (fromIntegral w) (fromIntegral h) wtm)

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
armarIm w h  wtm =if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
                 else bordeSuperior w
                      <->(printWTM wtm)
                      <->string def_attr (stLine wtm)
                      <->bordeInferior w

--Function that sets the string holding in the status line
setSt stl wtm = wtm {stLine = stl}

--Function that paints the command line with the accumulative string given
armarCommand s w h wtm = if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
                         else (printWTM wtm) <-> string def_attr s
                          
bordeSuperior w = char_fill dum '-' w 1
bordeInferior w = char_fill dum '-' w 2
-- TODO: mirar...
printNoWin w h |(w<1) || (h<1) = empty_image
               | otherwise = (string def_attr s <|> char_fill def_attr '#' (w-length s) 1) <-> char_fill dum '-' w (h-2)
                    where
                        s = take w "Ventana vacia. Utilice el comando TODO :agregar comando"
printWin w h = printNoWin w h

printWTM wtm = {-printControl (lo wtm) <-> -} runReader (printLayout (lo wtm)) (bm wtm)

printControl lOut = string def_attr $show lOut
printLayout :: Layout -> Reader BManager Image
printLayout lOut = case lOut of
                        NoWin (x,y) -> return $ printNoWin x y
                        (Window (x,y) num) -> return $ printWin x y
                        (Hspan x y xsL) -> mapM (\(lo,h)->printLayout lo) xsL >>= foldM (\a b -> return (a <|> barraVert y <|> b)) empty_image
                        (Vspan x y xsL) -> mapM (\(lo,h)->printLayout lo) xsL >>= foldM (\a b -> return (a <-> barraHoriz x <-> b)) empty_image
    where barraVert y = char_fill cbarras '|' 1 y
          barraHoriz x = char_fill cbarras '-' x 1

--Imprimir un buffer en una ventana

--Abrir ventana nueva
newWin :: Bool {- horizontal? -} -> WTManager -> IO WTManager
newWin horiz wtm = do
                (newBM,bnum) <- newBuffer (bm wtm) Nothing
                (DisplayRegion w h) <- display_bounds (terminal $vty wtm)
                newWTM <- return $ wtm{lo = splitLoX horiz (lo wtm) (curwdw wtm),curwdw = (\cur -> if (length cur) == 1 then [1,0] else (tail ( tail cur))++[1+last (tail cur),0]) (curwdw wtm)}
                lWTM <- return $ assocBuffer bnum (curwdw wtm) $ resizeLayout (fromIntegral w) (fromIntegral h) wtm
                showWTM lWTM
                return lWTM

--Asociar buffer a ventana
assocBuffer :: Int -> [Int] -> WTManager -> WTManager
assocBuffer bId wId wtm = wtm{lo = asBuf bId wId (lo wtm)}

asBuf :: Int -> [Int] -> Layout -> Layout
asBuf bId (w:ws) lo = case lo of
                        (Window (x,y) z) -> Window (x,y) bId
                        (NoWin (x,y)) -> Window (x,y) bId
                        (Vspan w h lst) -> Vspan w h (take w lst ++ [(\(l,s) -> (asBuf bId ws l,s)) (lst!!w)] ++ drop (w+1) lst)
                        (Hspan w h lst) -> Hspan w h (take w lst ++ [(\(l,s) -> (asBuf bId ws l,s)) (lst!!w)] ++ drop (w+1) lst)

--Hacer split Horizontal con param = True, vertical con param = False
splitX :: Bool -> WTManager -> WTManager
splitX param wtm = wtm{ lo = splitLoX param (lo wtm) cw, curwdw = (\xs -> init xs ++ (map (+1) [last xs])) cw } where cw = curwdw wtm

splitLoX :: Bool -> Layout -> [Int] -> Layout
splitLoX param l (x: xs@(y':ys)) = case l of
                (Vspan w h lst) -> Vspan w h ((take x) lst ++ [(\(lay,height) -> (splitLoX param lay xs,height)) (lst!!x)] ++ drop (x+1) lst)
                (Hspan w h lst) -> Hspan w h ((take x) lst ++ [(\(lay,width) -> (splitLoX param lay xs,width)) (lst!!x)] ++ drop (x+1) lst)
                (Window (x,y) z) -> undefined -- Window (x,y) z
                (NoWin (x,y)) ->  undefined -- NoWin (x,y)
splitLoX param l [x] |param = case l of
                                (Vspan w h lst) -> Vspan w h (take x lst ++ [(splitSpan param (lst!!x) w)] ++ (drop (x+1) lst))
                                (Hspan w h lst) -> Hspan w h (map (\(l,s) -> (resizeLo w h l,s)) (take x lst ++ [(NoWin (0,0),0)] ++  drop x lst))
                                (Window (w,h) b)-> resizeLo w h $ Hspan w h [(Window (w,h) b,1),(NoWin (0,0),0)]
                     |not param = case l of
                                (Vspan w h lst) -> Vspan w h (map (\(l,s) -> (resizeLo w h l,h)) (take x lst ++ [(NoWin (0,0),0)] ++ drop x lst))
                                (Hspan w h lst) -> Hspan w h (take x lst ++ [(splitSpan param (lst!!x) h)] ++ (drop (x+1) lst))
                                (Window (w,h) b) -> resizeLo w h $ Vspan w h [(Window (w,h) b,1),(NoWin (0,0),0)] 
                                (NoWin (w,h)) -> resizeLo w h $ Vspan w h [(NoWin (w,h),1),(NoWin (w,h),1)]
    where splitSpan param (lo,s) t |param = (resizeLo t s (Hspan t s [(lo,1),(NoWin (1,1), 1)]),t)
                                   |not param = (resizeLo s t (Vspan s t [(lo,1),(NoWin (1,1), 1)]),t)
