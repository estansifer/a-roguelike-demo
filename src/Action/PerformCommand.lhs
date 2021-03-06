
> module Action.PerformCommand (
>       perform_command
>   ) where

> import Control.Monad (when, forM_, unless)
> import qualified Data.Array.IArray as IA

> import Util.Util (db)
> import Util.RandomM
> import Defs
> import Output
> import TerrainComputation
> import PlayerCommand
> import State.Species
> import State.Creature
> import State.Player
> import State.State
> import Action.Creatures
> import Action.Player
> import Action.Initialization
> import Action.SpawnMonster
> import Action.Thread
> import Action.Attack
> import Action.Monster


> perform_command :: PlayerCommand -> U ()
> perform_command pc = do
>   legal <- is_legal_command pc
>   when legal (perform_legal_command pc >> player_tick)

> is_legal_command :: PlayerCommand -> U Bool
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
>           RefreshScreen -> False
>           Quit -> False)


> perform_legal_command :: PlayerCommand -> U ()
> perform_legal_command pc = do
>   case pc of
>       Move dir -> go_in_direction dir
>       Drink -> drink
>       Read -> scroll >> update_arrays >> pick_up_objects
>       Down -> end_level
>   age_player

> go_in_direction :: Dir -> U ()
> go_in_direction dir =
>   if dir == (0, 0) then pick_up_objects else do
>   loc <- get_player_location
>   m_cid <- get_cid_at (loc `add_dir` dir)
>   case m_cid of
>       Just cid -> player_attack cid
>       Nothing -> move dir

> move :: Dir -> U ()
> move dir = do
>   loc <- get_player_location
>   move_player_to (loc `add_dir` dir)
>   update_arrays
>   pick_up_objects

> pick_up_objects :: U ()
> pick_up_objects = do
>   objects <- get_objects
>   loc <- get_player_location
>
>   let remove = filter (Stairs /=) (objects IA.! loc)
>   let stay   = filter (Stairs ==) (objects IA.! loc)
>   unless (null remove) $ do
>       mapM_ pick_up_object remove
>       set_objects $ objects IA.// [(loc, stay)]

> player_tick :: U ()
> player_tick = move_normal_monsters >> maybe_spawn_normal_monsters

> move_normal_monsters :: U ()
> move_normal_monsters = do
>   creatures <- get_living_creatures
>   forM_ creatures $ \creature -> do
>       cid <- get_cid creature
>       forM_ (movement_type $ species creature) $ \mt -> case mt of
>           WithHuman -> perform_monster_action_if_alive cid
>           WithHumanSlow p -> wbranch p (perform_monster_action_if_alive cid) (return ())
>           _ -> return ()
