
> module Action.PerformCommand (
>       perform_command
>   ) where

> import Defs
> import TerrainComputation
> import PlayerCommand
> import State.Player
> import State.State
> import State.MState
> import Action.Player
> import Action.Initialization
> import Action.SpawnMonster


> perform_command :: PlayerCommand -> GS ()
> perform_command !pc = do
>   legal <- is_legal_command pc
>   if legal then perform_legal_command pc else return ()

> is_legal_command :: PlayerCommand -> GS Bool
> is_legal_command pc = do
>   a <- alive
>   if not a then return False else do
>       valid_dirs <- get_valid_dirs
>       loc <- get_player_location
>       player <- get_player
>       objects <- get_objects
>
>       return (case pc of
>           Move dir -> dir `elem` (valid_dirs IA.! loc)
>           Drink -> has_potion player
>           Read -> has_scroll player
>           Down -> Stairs `elem` (objects IA.! loc)
>           Quit -> False))


> perform_legal_command :: PlayerCommand -> GS ()
> perform_legal_command pc = do
>   case pc of
>       Move dir -> move dir
>       Drink -> drink
>       Read -> scroll
>       Down -> descend_level -- TODO
>       Quit -> error "Quit is not a legal action"
>   age_normal_monsters
>   age_player
>   maybe_spawn_normal_monsters

> move :: Dir -> GS ()
> move dir = do
>   loc <- location
>   move_player_to (loc `add_dir` dir)
>   update_arrays
>   pick_up_objects

> pick_up_objects :: GS ()
> pick_up_objects = do
>   objects <- get_objects
>   loc <- get_player_location
>
>   let remove = filter (Stairs /=) (objects IA.! loc)
>   let stay   = filter (Stairs ==) (objects IA.! loc)
>   if null remove then return () else do
>       modify_player (flip pick_up_objs remove)
>       set_objects $ objects IA.// [(loc, stay)]

> age_normal_monsters :: GS ()
> age_normal_monsters = do
>   cids <- get_cids_by_movement WithHuman
>   mapM_ perform_monster_action cids
