
> module Action.Initialization (
>       initialize_state,
>       create_character,
>       initialize_level,
>       update_arrays
>   ) where

> import qualified Data.Array.IArray as IA
> import qualified Data.Ix as Ix

> import Util.Util (arrayize)
> import Constants
> import TerrainComputation
> import Create.Terrain
> import Create.Objects
> import State.Species
> import State.Player
> import State.State
> import State.MState
> import Action.Pause
> import Action.SpawnMonster
> import Action.Player
> import Action.Creatures

> initialize_state :: GS ()
> initialize_state = do
>   params <- get_parameters
>   let bounds = ((1, 1), params)
>   
>   set_state $ State {
>           dims_ = params,
>           bounds_ = bounds,
>           all_positions_ = Ix.range bounds,
>           dungeon_depth_ = starting_depth
>       }
>   new_creatures

> create_character :: GS ()
> create_character = do
>   cid <- create_player
>   set_player $ new_player cid


We populate the monsters after placing the character so that
none are placed in line-of-sight of the player.

> initialize_level :: GS ()
> initialize_level = do
>   make_new_terrain
>   populate_objects
>   reset_creatures
>   place_character
>   update_arrays
>   populate_monsters
>   new_paused_switch

> make_new_terrain :: GS ()
> make_new_terrain = do
>   dims <- get_dims
>   bounds <- get_bounds
>   terrain <- create_terrain_m dims
>
>   set_terrain terrain
>   set_valid_dirs (compute_valid_dirs terrain)
>   let los = arrayize (const False) bounds
>   set_kaart los
>   set_line_of_sight los

> populate_objects :: GS ()
> populate_objects = do
>   terrain <- get_terrain
>   objects <- create_objects_m terrain
>   set_objects objects

> place_character :: GS ()
> place_character = do
>   terrain <- get_terrain
>   loc <- random_open_location_m terrain
>   move_player_to loc

> update_arrays :: GS ()
> update_arrays = do
>   bounds <- get_bounds
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
