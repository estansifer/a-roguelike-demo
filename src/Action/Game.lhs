
> module Action.Game (
>       main_game
>   ) where

> import Control.Monad (unless)
> import Control.Concurrent

> import Util.Util (repeat_until, db)
> import Util.InputStream
> import Util.Stream
> import Util.Flag
> import Constants
> import State.Species
> import State.Creature
> import State.Player
> import State.State
> import Action.Thread
> import Action.Initialization
> import Action.PerformCommand
> import Action.SpawnMonster
> import Action.Creatures
> import Output
> import PlayerCommand


> main_game :: L ()
> main_game = do
>   input_stream <- liftIO input_stream_char
>   play_level input_stream new_player new_player_creature starting_depth


TODO:  guarantee that new active threads cannot be registered
after the game has been paused

> play_level :: Stream Char -> Player -> Creature -> Integer -> L ()
> play_level input_stream p c d = do
>   lock $ create_level p c d
>   continue_level input_stream

> continue_level :: Stream Char -> L ()
> continue_level input_stream = do
>   start_clock
>   process_player_commands input_stream
>   lock repaint
>   lock unpause
>   block_until_paused

Block until all threads are done.

>   ats <- lock get_active_threads
>   liftIO $ mapM_ block_on_flag ats

>   q <- asks (quit_game . switching) >>= liftIO . is_raised
>   unless q $ do
>       p <- lock (get_player)
>       c <- lock (get_player_creature)
>       d <- lock (get_depth)
>       play_level input_stream p c (d + 1)


> start_clock :: L ()
> start_clock = fork_thread $ do
>   clock_speed <- asks (clock_speed . constants)
>   repeat_until_paused $ do
>       lock repaint
>       liftIO $ threadDelay clock_speed
>       unless_paused clock_tick


> process_player_commands :: Stream Char -> L ()
> process_player_commands input_stream = fork_thread $ do
>   liftIO $ drop_pending_values input_stream
>   repeat_until_paused $ do
>       liftIO $ block_until_ready input_stream
>       unless_paused $ process_command input_stream

> process_command :: Stream Char -> U ()
> process_command input_stream = do
>   command <- liftIO $ next_command input_stream
>   case command of
>       Quit -> (asks (quit_game . switching) >>= liftIO . raise_flag) >> pause
>       RefreshScreen -> hard_refresh
>       _ -> perform_command command
>   repaint_force
