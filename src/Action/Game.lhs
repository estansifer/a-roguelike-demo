
> module Action.Game (
>       main_game
>   ) where

> import Control.Concurrent

> import Util.Util (loop)
> import Util.InputStream
> import Util.Stream
> import State.Species
> import State.State
> import State.MState
> import Action.Pause
> import Action.Initialization
> import Action.PerformCommand
> import Action.SpawnMonster
> import Output
> import PlayerCommand


> main_game :: GS ()
> main_game = do
>   initialize_game 
>   input_stream <- liftIO input_stream_char
>   main_loop input_stream


> initialize_game :: GS ()
> initialize_game = do
>   initialize_state
>   create_character

> main_loop :: Stream Char -> GS ()
> main_loop input_stream = loop $ do
>   initialize_level
>   start_timed_events
>   fork_gs $ process_player_commands input_stream
>   lock unpause
>   block_until_paused
>   increment_depth


> start_timed_events :: GS ThreadId
> start_timed_events = fork_gs $ do
>   clock_speed <- get_clock_speed
>   block_until_unpaused
>   repeat_until_paused $ do
>       liftIO $ threadDelay clock_speed
>       unless_paused maybe_spawn_timed_monsters


> process_player_commands :: Stream Char -> GS ()
> process_player_commands input_stream = do
>   block_until_unpaused
>   liftIO $ drop_pending_values input_stream
>   repeat_until_paused $ do
>       lock repaint
>       liftIO $ block_until_ready input_stream
>       unless_paused (liftIO (next_command input_stream) >>= perform_command)
