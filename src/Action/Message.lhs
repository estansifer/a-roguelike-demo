
> module Action.Message (
>       type_message
>   ) where

> import Control.Monad (unless)

> import Util.Stream
> import Util.CursesWrapper

> type_message :: Stream Char -> IO ()
> type_message input_stream = disp 0 '>' >> type_message_n 0 input_stream

> type_message_n :: Int -> Stream Char -> IO ()
> type_message_n x input_stream = do
>   c <- next_value input_stream
>   unless (c == '/') (disp x c >> type_message_n (x + 1) input_stream)

> disp :: Int -> Char -> IO ()
> disp x c = print_char x 0 c >> refresh
