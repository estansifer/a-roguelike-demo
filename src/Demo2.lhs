
> module Demo2 where
>
> import System.Random
> import Control.Concurrent.MVar
>
> import State.GameState
> import Util.InputStream
> import Output
> import Util.CursesWrapper
> import Util.Flag
> import PlayerCommand
> import Action.PerformCommand


> main :: IO ()
> main = wrap_main $ do
>   (x, y) <- get_screen_size
>   (signal_quit, block_on_quit) <- make_flag
>   input_stream <- input_stream_char
>   run_game (x, y - 1) $ initialize_game >> main_loop input_stream signal_quit
>   block_on_quit

Note that even on illegal commands the screen repaints.

> main_loop :: MVar Char -> IO () -> GS ()
> main_loop input_stream signal_quit = do
>   repaint
>   pc <- liftIO $ next_command input_stream
>   if pc == Quit
>       then liftIO signal_quit
>       else perform_command_if_legal pc >> main_loop input_stream signal_quit
