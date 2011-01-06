
> module State.MState (
>       get_dims,
>       get_bounds,
>       get_all_positions,
>       get_dungeon_depth,
>       get_terrain,
>       get_valid_dirs,
>       get_kaart,
>       get_objects,
>       get_creatures_data,
>       get_creatures,
>       get_player,
>       get_location,
>       get_line_of_sight,
>       get_shortest_paths,
>       get_paused_switch,
>
>       set_kaart,
>       set_objects,
>       set_creatures_data,
>       set_player,
>       set_location,
>       set_line_of_sight,
>       set_shortest_paths,
>       set_paused_switch,
>
>       increment_depth,
>
>       status_line
>   ) where

> import qualified Data.Array.IArray as IA
> import qualified Ix
>
> import Defs
> import Util.RandomM
> import Create.CreateTerrain
> import Create.CreateObjects
> import TerrainComputation
> import State.Player
> import State.State
> import State.Creature




> get_ :: (State -> a) -> GS a
> get_ f = fmap f get_state


> get_dims :: GS Pos
> get_dims = get_ dims_

> get_bounds :: GS (Pos, Pos)
> get_bounds = get_ bounds_

> get_all_positions :: GS [Pos]
> get_all_positions = get_ all_positions_

> get_dungeon_depth :: GS Int
> get_dungeon_depth = get_ dungeon_depth_

> get_terrain :: GS Terrain
> get_terrain = get_ terrain_

> get_valid_dirs :: GS ValidDirs
> get_valid_dirs = get_ valid_dirs_

> get_objects :: GS Objects
> get_objects = get_ objects_

> get_creatures_data :: GS Creatures
> get_creatures_data = get_ creatures_

> get_creatures :: GS [Creature]
> get_creatures = get_ (creatures_list . creatures_)

> get_kaart :: GS Kaart
> get_kaart = get_ kaart_

> get_player :: GS CID
> get_player = get_ player_

> get_location :: GS Pos
> get_location = get_ player_location_

> get_line_of_sight :: GS LOS
> get_line_of_sight = get_ line_of_sight_

> get_shortest_paths :: GS Pathing
> get_shortest_paths = get_ shortest_paths_

> get_paused_switch :: GS Flag
> get_paused_switch = get_ paused_switch_


> is_alive :: GS Bool
> is_alive = get_ (alive . player_)







> set_depth :: Int -> GS ()
> set_depth n = modify_state (\s -> s {dungeon_depth_ = n})

> set_terrain :: Terrain -> GS ()
> set_terrain t = modify_state (\s -> s {terrain_ = t})

> set_valid_dirs :: ValidDirs -> GS ()
> set_valid_dirs vd = modify_state (\s -> s {valid_dirs_ = vd})

> set_kaart :: Kaart -> GS ()
> set_kaart k = modify_state (\s -> s {kaart_ = k})


> set_objects :: Objects -> GS ()
> set_objects o = modify_state (\s -> s {objects_ = o})

> set_creatures_data :: Creatures -> GS ()
> set_creatures_data c = modify_state (\s -> s {creatures_ = c})

TODO -- there's more to it than just this

> set_location :: Pos -> GS ()
> set_location l = error "unimplemented" -- modify_state (\s -> s {player_location_ = l})

> set_line_of_sight :: LOS -> GS ()
> set_line_of_sight los = modify_state (\s -> s {line_of_sight_ = los})

> set_shortest_paths :: Pathing -> GS ()
> set_shortest_paths p = modify_state (\s -> s {shortest_paths_ = p})

> set_paused_switch :: Flag -> GS ()
> set_paused_switch p = modify_state (\s -> s {paused_switch_ = p})


> increment_depth :: GS ()
> increment_depth = dungeon_depth >>= set_depth . (+1)

> set_random_terrain :: GS ()
> set_random_terrain = dims >>= set_terrain . create_terrain_m

> set_random_location :: GS ()
> set_random_location = (terrain >>= random_empty_location_m) >>= set_location




** Status line **

> status_line :: GS String
> status_line = get_ status_line_pure

> status_line_pure :: State -> String
> status_line_pure os = let p = player_ os in
>       "    > " ++ show (dungeon_depth_ os) ++
>       "    @ " ++ show (health p) ++ "/" ++ show (max_health p) ++
>       "    | " ++ show (xp_level p) ++ ":" ++ show (xp p) ++
>       "    ! " ++ show (num_potions (inventory p)) ++
>       "    ? " ++ show (num_scrolls (inventory p)) ++
>       "    ; " ++ show (hunger p)
