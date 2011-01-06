
> module Action.Game (
>       main_game
>   ) where

> import Control.Concurrent

> import Util.Util (repeat_until, db)
> import Util.InputStream
> import Util.Stream
> import Util.Flag
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
>   quit <- liftIO new_flag
>   main_loop input_stream quit


> initialize_game :: GS ()
> initialize_game = do
>   initialize_state
>   create_character

> main_loop :: Stream Char -> Flag -> GS ()
> main_loop input_stream quit = repeat_until
>   (do
>       db "a"
>       initialize_level
>       db "b"
>       start_timed_events
>       db "c"
>       fork_gs $ process_player_commands input_stream quit
>       db "d"
>       lock unpause            -- blocks forever here sometimes
>       db "e"
>       block_until_paused     -- blocks forever here sometimes
>       db "f"
>       increment_depth
>       db "g")
>   (\_ -> db "h" >> (liftIO $ is_raised quit >>= \b -> (db "i" >> return b)))



> start_timed_events :: GS ThreadId
> start_timed_events = fork_gs $ do
>   clock_speed <- get_clock_speed
>   block_until_unpaused
>   repeat_until_paused $ do
>       liftIO $ threadDelay clock_speed
>       unless_paused maybe_spawn_timed_monsters


> process_player_commands :: Stream Char -> Flag -> GS ()
> process_player_commands input_stream quit = do
>   block_until_unpaused
>   liftIO $ drop_pending_values input_stream
>   repeat_until_paused $ do
>       lock repaint_force
>       liftIO $ block_until_ready input_stream
>       unless_paused $ process_command input_stream quit

> process_command :: Stream Char -> Flag -> GS ()
> process_command input_stream quit = do
>   command <- liftIO $ next_command input_stream
>   case command of
>       Quit -> liftIO (raise_flag quit) >> pause
>       _ -> perform_command command
