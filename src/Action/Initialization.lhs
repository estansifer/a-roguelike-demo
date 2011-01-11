
> module Action.Initialization (
>       create_level,
>       update_arrays
>   ) where

> import qualified Data.IntMap as IM
> import qualified Data.Array.IArray as IA
> import qualified Data.Array.MArray as MA
> import qualified Data.Ix as Ix

> import Util.Util (arrayize, db)
> import Constants
> import Output
> import TerrainComputation
> import Create.Terrain
> import Create.Objects
> import State.Species
> import State.Creature
> import State.Player
> import State.State
> import Action.Thread
> import Action.SpawnMonster
> import Action.Player
> import Action.Creatures

We populate the monsters after placing the character so that
none are placed in line-of-sight of the player.

> create_level :: Player -> Creature -> Integer -> U ()
> create_level p c d = do
>   set_depth d
>   set_player p
>   make_new_terrain
>   populate_objects
>   create_creatures c
>   populate_monsters
>   update_arrays
>   new_paused_switch
>   set_active_threads []
>   hard_refresh        -- sets last repaint time

> make_new_terrain :: U ()
> make_new_terrain = do
>   dims <- asks (dimensions . constants)
>   bounds <- asks (bounds . constants)
>   terrain <- create_terrain_m dims
>
>   set_terrain terrain
>   set_valid_dirs (compute_valid_dirs terrain)
>   let los = arrayize (const False) bounds
>   set_kaart los
>   set_line_of_sight los

> populate_objects :: U ()
> populate_objects = do
>   terrain <- get_terrain
>   objects <- create_objects_m terrain
>   set_objects objects

> create_creatures :: Creature -> U ()
> create_creatures pc = do
>   bounds <- asks (bounds . constants)
>   terrain <- get_terrain
>   loc <- random_open_location_m terrain
>
>   let pcid = 1
>
>   l_map <- liftIO $ MA.newArray bounds Nothing
>
>   liftIO $ MA.writeArray l_map loc (Just pcid)
>
>   set_creatures $ Creatures {
>           cid_map = IM.singleton pcid (pc {location = loc}),
>           loc_map = l_map,
>           player_cid = pcid,
>           next_cid = pcid + 1
>       }

> update_arrays :: U ()
> update_arrays = do
>   bounds <- asks (bounds . constants)
>   terrain <- get_terrain
>   valid_dirs <- get_valid_dirs
>   location <- get_player_location
>   old_kaart <- get_kaart
>
>   let new_los = compute_los terrain location
>       f_new_kaart pos = (old_kaart IA.! pos) || (new_los IA.! pos)
>   set_kaart $ arrayize f_new_kaart bounds
>   set_line_of_sight new_los
>   set_shortest_paths (compute_shortest_paths valid_dirs location)
