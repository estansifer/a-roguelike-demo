
> module PlayerCommand (
>       PlayerCommand(..),
>       char_to_command,
>       next_command
>   ) where
>
> import Defs
> import Util.Stream

> data PlayerCommand =
>   Move Dir |
>   Drink |
>   Read |
>   Down |
>   RefreshScreen |
>   Quit
>       deriving (Eq, Show)

> char_to_command :: Char -> Maybe PlayerCommand
> char_to_command c = case c of
>   'h' -> Just $ Move (-1,  0)
>   'j' -> Just $ Move ( 0,  1)
>   'k' -> Just $ Move ( 0, -1)
>   'l' -> Just $ Move ( 1,  0)
>   'y' -> Just $ Move (-1, -1)
>   'u' -> Just $ Move ( 1, -1)
>   'b' -> Just $ Move (-1,  1)
>   'n' -> Just $ Move ( 1,  1)
>   '.' -> Just $ Move ( 0,  0)
>
>   'd' -> Just Drink
>   'r' -> Just Read
>   '>' -> Just Down
>
>   'R' -> Just RefreshScreen
>
>   'Q' -> Just Quit
>
>   _ -> Nothing

Blocks until a command is received on input.

> next_command :: Stream Char -> IO PlayerCommand
> next_command input_stream = do
>   c <- next_value input_stream
>   case char_to_command c of
>       Just pc -> return pc
>       Nothing -> next_command input_stream
