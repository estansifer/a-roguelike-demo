
> module Util.InputStream (
>       input_stream_char, input_stream_key, input_stream_any_key
>   ) where
>
> import Control.Monad (when, unless)
>
> import Util.Stream
> import Util.CursesWrapper (get_char, get_key, get_any_key, print_char, refresh)
> import UI.HSCurses.Curses (Key)
> import qualified UI.HSCurses.Curses as C

All of the following functions use curses library calls,
and must be called from within the 'wrap_main' wrapper.



Returns an mvar such that every time the user presses a key,
the character representing it is put into the specified mvar.
The next character is not put into the mvar until the previous
has been consumed.

This creates a thread that is run until execution of the
program ends.  I do not believe it is safe to take any input
except through the returned stream once one of these functions
has been called.

> input_stream_char :: IO (Stream Char)
> input_stream_char = make_stream get_char

> get_char'' :: IO Char
> get_char'' = do
>   c <- get_char
>   if c == '/' then print_message0 >> get_char' else return c

> print_message0 :: IO ()
> print_message0 = print_char 0 0 '>' >> refresh >> print_message0' 0

> print_message0' :: Int -> IO ()
> print_message0' x = do
>   c <- get_char
>   if c == '/'
>       then return ()
>       else print_char x 0 c >> refresh >> print_message0' (x + 1)

> get_char' :: IO Char
> get_char' = do
>   k <- get_key
>   when (k == C.KeyEnter) print_message
>   case k of
>       C.KeyChar c -> return c
>       _ -> get_char'

> print_message :: IO ()
> print_message = print_char 0 0 '>' >> refresh >> print_message' 0

> print_message' :: Int -> IO ()
> print_message' x = do
>   k <- get_key
>   unless (k == C.KeyEnter)
>       (case k of
>           C.KeyChar c -> print_char x 0 c >> refresh >> print_message' (x + 1)
>           _ -> print_message' x)


> input_stream_key :: IO (Stream Key)
> input_stream_key = make_stream get_key

> input_stream_any_key :: IO (Stream ())
> input_stream_any_key = make_stream get_any_key
