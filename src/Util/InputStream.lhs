
> module Util.InputStream (
>       input_stream_char, input_stream_key, input_stream_any_key
>   ) where
>
> import Util.Stream
> import Util.CursesWrapper (get_char, get_key, get_any_key)
> import UI.HSCurses.Curses (Key)

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

> input_stream_key :: IO (Stream Key)
> input_stream_key = make_stream get_key

> input_stream_any_key :: IO (Stream ())
> input_stream_any_key = make_stream get_any_key
