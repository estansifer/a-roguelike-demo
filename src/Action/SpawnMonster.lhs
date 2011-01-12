
> module Action.SpawnMonster (
>       populate_monsters,
>       spawn_monster,
>       clock_tick,
>       maybe_spawn_timed_monsters,
>       maybe_spawn_normal_monsters
>   ) where
>
> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM
>
> import Data.List (intersect)
> import Control.Monad (forM_, when)
>
> import Util.Util (repeat_until, db)
> import Util.RandomM
> import Util.Flag (regular_delay, make_sine_delay)
> import TerrainComputation
> import Constants
> import Defs
> import Output
> import State.Species
> import State.State
> import State.Creature
> import Action.Creatures
> import Action.Monster
> import Action.Thread

> populate_monsters :: U ()
> populate_monsters = do
>   (dx, dy) <- asks (dimensions . constants)
>   let area = fromIntegral (dx * dy)
>   cur_depth <- get_depth
>   forM_ all_species $ \species ->
>       when (cur_depth > min_depth species) $ sequence_ $ replicate
>           (fromIntegral ((area * scarcity cur_depth species) `div` scarcity_per))
>           (spawn_monster species)

> spawn_monster :: Species -> U ()
> spawn_monster species = do
>   los <- get_line_of_sight
>   terrain <- get_terrain
>   pos <- repeat_until
>           (random_open_location_m terrain)
>           (\p -> do
>               e <- is_empty_pos p
>               return (e && (not (los IA.! p))))
>   cid <- create_creature_at species pos
>   forM_ (movement_type species) $ \mt ->
>       when (timed mt) (fork_creature_movement mt cid)

> fork_creature_movement :: MovementType -> CID -> U ()
> fork_creature_movement mt cid = do
>   delay <- movement_delay mt
>   halt <- lift $ repeat_with_delay
>           delay
>           (perform_monster_action_if_alive cid)
>   modify_creature cid $ register_kill_listener halt

> movement_delay :: MovementType -> U (L ())
> movement_delay mt = case mt of
>   Timed d -> return (regular_delay d)
>   TimedWave d p a -> fmap liftIO $ make_sine_delay d p a


Called once per time tick

> clock_tick :: U ()
> clock_tick = maybe_spawn_timed_monsters >> repaint

> maybe_spawn_timed_monsters :: U ()
> maybe_spawn_timed_monsters = maybe_spawn_monsters_by_species $
>   filter (any timed . movement_type) all_species

Called once per human action tick

> maybe_spawn_normal_monsters :: U ()
> maybe_spawn_normal_monsters = maybe_spawn_monsters_by_species $
>   filter (any (not . timed) . movement_type) all_species

> maybe_spawn_monsters_by_species :: [Species] -> U ()
> maybe_spawn_monsters_by_species ss = do
>   (dx, dy) <- asks (dimensions . constants)
>   let area = fromIntegral (dx * dy)
>   cur_depth <- get_depth
>   forM_ ss $ \species ->
>       when (cur_depth >= min_depth species) $ do
>           r <- randomR (0, scarcity_per * scarcity_regen_time)
>           when (r < area * scarcity cur_depth species) (spawn_monster species)
