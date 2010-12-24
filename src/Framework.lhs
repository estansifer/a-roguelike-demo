
> module Framework where
>
> import Data.IORef
> import Control.Concurrent
> import Control.Concurrent.MVar
>
> import Util.Util (loop)
> import Util.CursesWrapper
> import Util.Signal
> import Util.InputStream (input_stream_char)

> data GameState = GS

> type GameStateVar = MVar GameState

> data PlayerCommand = PC

> main :: IO ()
> main = wrap_main $ do
>   gs <- make_game_state
>   gsv <- newMVar gs
>   main_loop gsv

> main_loop :: GameStateVar -> IO ()
> main_loop gsv = do
>   input <- input_stream_char
>   move_monsters_on_signal gsv
>   loop $ do
>       c <- takeMVar input
>       note ("Player pushed: \"" ++ [c] ++ "\"")
>       move_player gsv PC

> note :: String -> IO ()
> note = putStrLn

> make_game_state :: IO GameState
> make_game_state = do
>   note "Making new game state"
>   return GS

> move_monsters_on_signal :: GameStateVar -> IO ThreadId
> move_monsters_on_signal gsv = forkIO $ do
>   signal <- infinite_signal_ (3 * one_second)
>   loop $ (takeMVar signal >> move_timed_monsters gsv)

> move_timed_monsters :: GameStateVar -> IO ()
> move_timed_monsters gsv = do
>   gs <- takeMVar gsv
>   note "Moving timed monsters"
>   putMVar gsv gs
>   return ()

> move_normal_monsters :: GameStateVar -> IO ()
> move_normal_monsters gsv = do
>   gs <- takeMVar gsv
>   note "Moving normal monsters"
>   putMVar gsv gs
>   return ()

> move_player :: GameStateVar -> PlayerCommand -> IO ()
> move_player gsv pc = do
>   gs <- takeMVar gsv
>   note "Moving player"
>   putMVar gsv gs
>   return ()
