
> module Demo2 where
>
> import System.Random
> import Control.Concurrent.MVar
>
> import GameState
> import Util.InputStream
> import Output
> import Util.CursesWrapper
> import Util.Flag
> import PlayerCommand
> import PerformCommand


> main :: IO ()
> main = wrap_main $ do
>   seed <- randomIO
>   (x, y) <- get_screen_size
>   start_game (create_game x (y-1) seed)

> start_game :: GameState -> IO ()
> start_game gs = do
>   (signal_quit, block_on_quit) <- make_flag
>   input_stream <- input_stream_char
>   main_loop input_stream signal_quit gs
>   block_on_quit

Note that even on illegal commands the screen repaints.

> main_loop :: MVar Char -> IO () -> GameState -> IO ()
> main_loop input_stream signal_quit gs = do
>   repaint gs
>   pc <- next_command input_stream
>   if pc == Quit then signal_quit else
>       main_loop input_stream signal_quit $
>       if is_legal_command gs pc
>           then perform_command gs pc
>           else gs
