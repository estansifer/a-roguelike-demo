
> module Demo3 where

> import Util.CursesWrapper
> import State.State
> import Action.Game

> main :: IO ()
> main = wrap_main $ do
>   (x, y) <- get_screen_size
>   run_game (x, y) main_game
