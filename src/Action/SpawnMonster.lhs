
> module Action.SpawnMonster (
>       populate_monsters,
>       spawn_monster,
>       maybe_spawn_timed_monsters,
>       maybe_spawn_normal_monsters
>   ) where
>
> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM
>
> import Util.Util (repeat_until)
> import Constants
> import Defs
> import State.State
> import State.MState
> import State.Creature
> import Action.State
> import Action.Creature

> populate_monsters :: GS ()
> populate_monsters = do
>   (dx, dy) <- get_dims
>   let area = dx * dy
>   cur_depth <- get_depth
>   forM_ all_species $ \species ->
>       if cur_depth <= min_depth species then return () else
>       sequence $ replicate
>           ((area * scarcity species) `div` scarcity_per)
>           (spawn_monster species)

> spawn_monster :: Species -> GS ()
> spawn_monster species = do
>   los <- get_line_of_sight
>   terrain <- get_terrain
>   pos <- repeat_until (\p -> do
>               e <- is_empty_pos p
>               return (e && (not (los IA.! p)))) (random_empty_location_m terrain)
>   cid <- create_creature_at species pos
>   forM_ [Timed, TimedWave] $ \mt ->
>       if mt `elem` (movement_type species)
>           then fork_creature_movement mt cid
>           else return ()

> fork_creature_movement :: MovementType -> CID -> GS ()
> fork_creature_movement mt cid = do
>   clock_speed <- get_clock_speed
>   halt <- start_movement mt clock_speed (unless_paused $ perform_monster_action cid)
>   modify_creature cid $ register_kill_listener halt

> start_movement :: MovementType -> Int -> GS () -> GS (IO ())
> start_movement mt clock_speed action = case mt of
>   Timed -> regular_repeat_until_halted clock_speed action
>   TimedWave -> sine_repeat_until_halted clock_speed wave_period wave_amp action

Called once per time tick

> maybe_spawn_timed_monsters :: GS ()
> maybe_spawn_timed_monsters = maybe_spawn_monsters_by_species [Timed, TimedWave]

Called once per human action tick

> maybe_spawn_normal_monsters :: GS ()
> maybe_spawn_normal_monsters = maybe_spawn_monsters_by_species [WithHuman]

> maybe_spawn_monsters_by_species :: [Species] -> GS ()
> maybe_spawn_monsters_by_species ss = do
>   (dx, dy) <- get_dims
>   let area = dx * dy
>   cur_depth <- get_depth
>   forM_ ss $ \species ->
>       if cur_depth < min_depth species then return () else
>       r <- randomR (0, scarcity_per * scarcity_regen_time)
>       if r < area * scarcity species
>           then spawn_monster species
>           else return ()
