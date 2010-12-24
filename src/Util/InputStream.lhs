
> module Util.InputStream (
>       input_stream_char, input_stream_key, input_stream_any_key
>   ) where
>
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Util.CursesWrapper (get_char, get_key, get_any_key)
> import UI.HSCurses.Curses (Key)
> import Util.Util (loop)

All of the following functions use curses library calls,
and must be called from within the 'wrap_main' wrapper.

> _mk_stream :: IO a -> IO (MVar a)
> _mk_stream get = do
>   stream <- newEmptyMVar
>   forkIO $ loop $ (get >>= putMVar stream)
>   return stream

Returns an mvar such that every time the user presses a key,
the character representing it is put into the specified mvar.
The next character is not put into the mvar until the previous
has been consumed.

This creates a thread that is run until execution of the
program ends.  I do not believe it is safe to take any input
except through the returned stream once one of these functions
has been called.

> input_stream_char :: IO (MVar Char)
> input_stream_char = _mk_stream get_char

> input_stream_key :: IO (MVar Key)
> input_stream_key = _mk_stream get_key

> input_stream_any_key :: IO (MVar ())
> input_stream_any_key = _mk_stream get_any_key
