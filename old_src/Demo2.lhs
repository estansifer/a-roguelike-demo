
> module Demo2 where
>
> import System.Random
> import Control.Concurrent.MVar
>
> import State.State
> import Util.InputStream
> import Output
> import Util.CursesWrapper
> import Util.Signal
> import PlayerCommand
> import Action.PerformCommand


> main :: IO ()
> main = wrap_main $ do
>   (x, y) <- get_screen_size
>   quit_flag <- new_flag
>   input_stream <- input_stream_char
>   let quit = liftIO $ raise_flag quit_flag
>   run_game (x, y - 1) $ initialize_game >> main_loop input_stream quit
>   block_on_flag quit_flag

Note that even on illegal commands the screen repaints.

> main_loop :: MVar Char -> GS () -> GS ()
> main_loop input_stream quit = do
>   repaint
>   pc <- liftIO $ next_command input_stream
>   if pc == Quit
>       then quit
>       else perform_command_if_legal pc >> main_loop input_stream quit
