
{-#OPTIONS -XMultiParamTypeClasses #-}

module Main where
import BufferManager(BManager,BM,initBM)
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
                      ,vty :: Vty
                      }


newtype WTM a = WTM{runWTM :: WTManager -> (WTManager, a)}
instance Monad WTM where
 --(>>=) :: WTM a -> (a -> WTM b) -> WTM b
 wtm >>= op = WTM (\wtma -> let (interWTM,a) = runWTM wtm wtma in
                      runWTM (op a) interWTM)
 return a = WTM (\wtm -> (wtm,a))
 
 -- The way to initialize the window manager at program startup
 -- TODO: change wsizes, Window initializer, possibly add a startup
 -- size variable
initWTM :: BManager -> Vty -> WTManager
initWTM ref vty = WTMa {lo = Vspan 80 22 [(Hspan 80 10 [(Window (40,10) 1,40),(Window (39,10) 1, 40)],10),(Window (80,11) 1,11)]
                        ,curwdw = [0]
                        ,wtmH = 0
                        ,wtmW = 0
                        ,bm = ref
                        ,vty = vty
                        }

instance MonadState WTManager WTM where
    get = WTM (\wtma -> (wtma,wtma))
    put wtm = WTM (\wtma -> (wtm,()))
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
--To navigate through the layout
bgcolor = def_attr `with_fore_color` red
dum = def_attr `with_style` reverse_video
cbarras = def_attr `with_style` reverse_video `with_fore_color` red

--Function to resize the current layout to fit the new window size
resizeLayout::Int->Int->WTManager->WTManager
resizeLayout h w wtm = wtm{lo = (resizeLo h w) (lo wtm)}
resizeLo :: Int -> Int -> Layout -> Layout
resizeLo w h lo = case lo of
                    (Vspan px py xs) -> Vspan w h $(funcV w h xs)
                    (Hspan px py xs) -> Hspan w h $(funcH w h xs)
                    Window (xs,ys) bn -> Window (w,h) bn
                    NoWin (xs,ys) -> NoWin (w,h)
    where
        funcV w h lst = let (d,m) = divMod h (length lst) in map (\(l,lst) -> (resizeLo w (d+1) l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo w d l,d)) (drop m lst)
        funcH w h lst = let (d,m) = divMod w (length lst) in map (\(l,lst) -> (resizeLo (d+1) h l,d+1)) (take m lst) ++ map (\(l,lst) -> (resizeLo d h l,d)) (drop m lst)

main :: IO ()
main = mkVty >>= \vty -> 
        let wtm = initWTM initBM vty in
        next_event vty >>= \k -> 
        display_bounds ( terminal vty ) >>= \(DisplayRegion w h) ->
        printloop vty wtm w h >>
        shutdown vty

getKey :: WTManager -> IO Event
getKey wtm = next_event (vty wtm)

getCommand :: String -> WTManager -> Word -> Word -> IO ()
getCommand s wtm w h= do nEv <- next_event (vty wtm)
                         case nEv of
                           EvKey (KASCII c) _ -> let newS = s ++ [c] 
                                                 in do update (vty wtm) (pic_for_image $ armarCommand newS (fromIntegral w) (fromIntegral h) wtm)
                                                       getCommand newS wtm w h
                           EvKey (KEnter) _ -> update (vty wtm) (pic_for_image $ armarCommand "Ejecutando" (fromIntegral w) (fromIntegral h) wtm)
                           EvResize nx ny -> printloop (vty wtm) (resizeLayout nx ny wtm) (fromIntegral nx) (fromIntegral ny-1)
                           _ -> update (vty wtm) (pic_for_image $ armarCommand s (fromIntegral w) (fromIntegral h) wtm)
                      
printloop :: Vty -> WTManager -> Word -> Word -> IO ()
printloop vty wtm w h= do
                    update vty (pic_for_image $ armarIm (fromIntegral w) (fromIntegral h) wtm)
                    nextEV <- next_event vty
                    case nextEV of 
                        EvKey (KASCII 'q') [] -> return ()
                        EvKey (KASCII ':') _ -> getCommand ":" wtm w h
                        EvResize nx ny -> printloop vty (resizeLayout nx ny wtm) (fromIntegral nx) (fromIntegral ny)
                        _ -> printloop vty wtm w h

armarCommand s w h wtm = if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
                         else (printWTM wtm) <-> string def_attr s
                       

armarIm w h wtm =if h<4 then string def_attr "No se puede visualizar con una altura de menos de 4 caracteres"
                 else bordeSuperior w
                      <->(printWTM wtm)
                      <->bordeInferior w
{-

data WTManager = WTMa {lo :: Layout -- current layout
                      ,curwdw :: [Int] -- current window being used
                      ,wtmH :: Int -- height in characters of window
                      ,wtmW :: Int -- width in characters of window
                      ,bm :: BManager

data Layout = Vspan Int Int [(Layout,Int)] --Vertical span with sizes
             |Hspan Int Int [(Layout,Int)] --Horizontal span with sizes
             |Window {size::(Int,Int), buffNum::Int} -- Window with buffer number
             |NoWin {size:: (Int,Int)}

-}                     
bordeSuperior w = char_fill dum '-' w 1
bordeInferior w = char_fill dum '-' w 2
-- TODO: mirar...
printNoWin w h |(w<1) || (h<1) = empty_image
               | otherwise = (string def_attr s <|> char_fill def_attr '#' (w-length s) 1) <-> char_fill dum '-' w (h-1)
                    where
                        s = take w "Ventana vacia. Utilice el comando TODO :agregar comando"
printWin w h = printNoWin w h

printWTM wtm = printControl (lo wtm) <-> printLayout (lo wtm)

printControl lOut = string def_attr $show lOut
printLayout lOut = case lOut of
                        NoWin (x,y) -> printNoWin x y
                        (Window (x,y) num) -> printWin x y
                        (Hspan x y xsL) -> foldr1 (\a b -> a <|> barraVert y <|> b)  $ map (\(lo,h)->printLayout lo) xsL
                        (Vspan x y xsL) -> foldr1 (\a b -> a <-> barraHoriz x <-> b) $ map (\(lo,y)->printLayout lo) xsL
    where barraVert y = char_fill cbarras '|' 1 y
          barraHoriz x = char_fill cbarras '-' x 1

--Imprimir un buffer en una ventana

--Abrir ventana nueva

--Asociar buffer a ventana
assocBuffer :: Int -> [Int] -> WTManager -> WTManager
assocBuffer bId wId wtm = wtm{lo = asBuf bId wId (lo wtm)}
asBuf :: Int -> [Int] -> Layout -> Layout
asBuf bId (w:ws) lo = case lo of
                        (Window (x,y) z) -> Window (x,y) bId
                        (NoWin (x,y)) -> Window (x,y) bId
                        (Vspan w h lst) -> Vspan w h (take (w-1) lst ++ [(\(l,s) -> (asBuf bId ws l,s)) (lst!!w)] ++ drop w lst)
                        (Hspan w h lst) -> Hspan w h (take (w-1) lst ++ [(\(l,s) -> (asBuf bId ws l,s)) (lst!!w)] ++ drop w lst)
--Hacer split Horizontal con param = True, vertical con param = False
splitX :: Bool -> WTManager -> WTManager
splitX param wtm = wtm{ lo = splitLoX param (lo wtm) cw, curwdw = (\xs -> init xs ++ (map (+1) [last xs])) cw } where cw = curwdw wtm
splitLoX :: Bool -> Layout -> [Int] -> Layout
splitLoX param l (x: xs@(y':ys)) = case l of
                (Vspan w h lst) -> Vspan w h ((take (x-1)) lst ++ [(\(lay,height) -> (splitLoX param lay xs,height)) (lst!!x)] ++ drop x lst)
                (Hspan w h lst) -> Hspan w h ((take (x-1)) lst ++ [(\(lay,width) -> (splitLoX param lay xs,width)) (lst!!x)] ++ drop x lst)
                (Window (x,y) z) -> Window (x,y) z
                (NoWin (x,y)) ->  NoWin (x,y)
splitLoX param l [x] |param = case l of
                                (Vspan w h lst) -> Vspan w h (take (x-1) lst ++ [(splitSpan param (lst!!x) w)] ++ (drop x lst))
                                (Hspan w h lst) -> Hspan w h (map (\(l,s) -> (resizeLo w h l,s)) (take x lst ++ [(NoWin (0,0),0)] ++  drop x lst))
                                (Window (w,h) b)-> resizeLo w h $ Hspan w h [(Window (w,h) b,1),(NoWin (0,0),0)]
                     |not param = case l of
                                (Vspan w h lst) -> Vspan w h (map (\(l,s) -> (resizeLo w h l,h)) (take x lst ++ [(NoWin (0,0),0)] ++ drop x lst))
                                (Hspan w h lst) -> Hspan w h (take (x-1) lst ++ [(splitSpan param (lst!!x) h)] ++ (drop x lst))
                                (Window (w,h) b) -> resizeLo w h $ Vspan w h [(Window (w,h) b,1),(NoWin (0,0),0)] 
                                (NoWin (w,h)) -> resizeLo w h $ Vspan w h [(NoWin (w,h),1),(NoWin (w,h),1)]
    where splitSpan param (lo,s) t |param = (resizeLo t s (Hspan t s [(lo,1),(NoWin (1,1), 1)]),t)
                                   |not param = (resizeLo s t (Vspan s t [(lo,1),(NoWin (1,1), 1)]),t)
--Hacer split V
