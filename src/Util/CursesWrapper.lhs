
> {-# LANGUAGE FlexibleContexts #-}
>
> module Util.CursesWrapper (
>       wrap_main,
>       get_screen_size,
>       print_char,
>       print_string,
>       print_array_corner, print_array,
>       refresh,
>       get_char,
>       get_key,
>       get_any_key
>   ) where
>
> import Control.Monad (forM_)
> import Data.Array.IArray as IA
> import qualified UI.HSCurses.Curses as C
>
> wrap_main :: IO () -> IO ()
> wrap_main main' = do
>   C.initCurses
>   C.keypad C.stdScr True      -- make the cursor keys usable
>   C.echo False                -- disable echoing of key presses to screen
>   C.cursSet C.CursorInvisible
>   C.cBreak True               -- take key presses immediately
>   main'
>   C.endWin
>
> get_screen_size :: IO (Int, Int)
> get_screen_size = do
>   (y, x) <- C.scrSize
>   return (x, y)
>
> get_screen_width :: IO Int
> get_screen_width = C.scrSize >>= return . snd
>
> get_screen_height :: IO Int
> get_screen_height = C.scrSize >>= return . fst
>
> cast_char = toEnum . fromEnum
>
> print_char :: Int -> Int -> Char -> IO ()
> print_char x y c = C.mvAddCh y x (cast_char c)
>
> __print_string :: Int -> Int -> String -> IO ()
> __print_string x y s = do
>   max_x <- get_screen_width
>   if max_x <= x then return () else do
>       forM_ (zip (take (max_x - x) s) [x..]) $ \(c, x') ->
>           C.mvAddCh y x' (cast_char c)

The 'print_string' function does not work when the string reaches the lower-right
corner, so for that one character we need to use 'print_char'

> print_string :: Int -> Int -> String -> IO ()
> print_string x y s = do
>   (ymax, xmax) <- C.scrSize
>   if y < 0 || y >= ymax || x >= xmax then return () else
>       let x' = max 0 x
>           s' = if x < 0 then drop (-x) s else s
>           s''= take (xmax - x) s' in
>       if y + 1 == ymax && x + length s'' == xmax
>           then C.mvWAddStr C.stdScr y x' (init s'') >>
>                C.mvAddCh y (xmax - 1) (cast_char $ last s'')
>           else C.mvWAddStr C.stdScr y x' s''

Auxilliary function for 'print_array'.  Assumes the String fits entirely
within bounds, and that the given size of the screen is accurate.  It does
not do any checking of whether the string fits, just performs a special case on
printing to the lower-right corner (which crashes ordinarily).

> print_string_safe :: (Int, Int) -> Int -> Int -> String -> IO ()
> print_string_safe (xmax, ymax) x y s =
>   if y + 1 == ymax && x + length s == xmax
>       then C.mvWAddStr C.stdScr y x (init s) >>
>            C.mvAddCh y (xmax - 1) (cast_char $ last s)
>       else C.mvWAddStr C.stdScr y x s

> print_array_corner :: IA.IArray a Char => a (Int, Int) Char -> IO ()
> print_array_corner arr = print_array arr (0, 0)

Assumes the array fits entirely within bounds.  If not, it does nothing.

> print_array :: IA.IArray a Char => a (Int, Int) Char -> (Int, Int) -> IO ()
> print_array arr origin@(x0, y0) =
>   let ((xlow,ylow), (xhigh,yhigh)) = IA.bounds arr
>       xlen = xhigh - xlow + 1
>       ylen = yhigh - ylow + 1 in do
>   (xmax, ymax) <- get_screen_size
>   if x0+xlen > xmax || y0+ylen > ymax || x0 < 0 || y0 < 0 then return () else
>       forM_ [ylow..yhigh] $ \y ->
>           print_string_safe (xmax, ymax) x0 (y0+(y-ylow))
>                   [arr IA.! (x, y) | x <- [xlow..xhigh]]

Updates the display.

> refresh :: IO ()
> refresh = C.refresh

Blocks until a printable char is received on input.  Any non-printable chars
received on input are ignored.

> get_char :: IO Char
> get_char = do
>   k <- C.getCh
>   case k of
>       C.KeyChar c -> return c
>       _       -> get_char

Blocks until a keypress is received on input.  If printable, it is represented
as 'Just c' for some character 'c', if it is not printable it is represented
as 'Nothing'.

> get_key_as_char :: IO (Maybe Char)
> get_key_as_char = do
>   k <- C.getCh
>   case k of
>       C.KeyChar c -> return (Just c)
>       _       -> return Nothing

Blocks until a keypress is received on input, and returns the keypress.

> get_key :: IO C.Key
> get_key = C.getCh

Blocks until input is received, and returns nothing.

> get_any_key :: IO ()
> get_any_key = C.getCh >> return ()
