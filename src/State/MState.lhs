
> module State.MState (
>       get_dims,
>       get_bounds,
>       get_all_positions,
>       get_dungeon_depth,
>       get_terrain,
>       get_valid_dirs,
>       get_kaart,
>       get_objects,
>       get_creatures,
>       get_player,
>       get_player_location,
>       get_line_of_sight,
>       get_shortest_paths,
>       get_paused_switch,
>
>       set_terrain,
>       set_valid_dirs,
>       set_kaart,
>       set_objects,
>       set_creatures,
>       set_player,
>       set_player_location,
>       set_line_of_sight,
>       set_shortest_paths,
>       set_paused_switch,
>
>       increment_depth,
>
>       status_line
>   ) where

> import qualified Data.IntMap as IM

> import Util.Flag
> import Defs
> import State.Creature
> import State.Inventory
> import State.Player
> import State.Health
> import State.State




> get_ :: (State -> a) -> GS a
> get_ f = fmap f get_state


> get_dims :: GS Pos
> get_dims = get_ dims_

> get_bounds :: GS (Pos, Pos)
> get_bounds = get_ bounds_

> get_all_positions :: GS [Pos]
> get_all_positions = get_ all_positions_

> get_dungeon_depth :: GS Integer
> get_dungeon_depth = get_ dungeon_depth_

> get_terrain :: GS Terrain
> get_terrain = get_ terrain_

> get_valid_dirs :: GS ValidDirs
> get_valid_dirs = get_ valid_dirs_

> get_objects :: GS Objects
> get_objects = get_ objects_

> get_creatures :: GS Creatures
> get_creatures = get_ creatures_

> get_kaart :: GS Kaart
> get_kaart = get_ kaart_

> get_player :: GS Player
> get_player = get_ player_

> get_player_location :: GS Pos
> get_player_location = get_ player_location_

> get_line_of_sight :: GS LOS
> get_line_of_sight = get_ line_of_sight_

> get_shortest_paths :: GS Pathing
> get_shortest_paths = get_ shortest_paths_

> get_paused_switch :: GS Switch
> get_paused_switch = get_ paused_switch_








> set_dungeon_depth :: Integer -> GS ()
> set_dungeon_depth n = modify_state (\s -> s {dungeon_depth_ = n})

> set_terrain :: Terrain -> GS ()
> set_terrain t = modify_state (\s -> s {terrain_ = t})

> set_valid_dirs :: ValidDirs -> GS ()
> set_valid_dirs vd = modify_state (\s -> s {valid_dirs_ = vd})

> set_kaart :: Kaart -> GS ()
> set_kaart k = modify_state (\s -> s {kaart_ = k})


> set_objects :: Objects -> GS ()
> set_objects o = modify_state (\s -> s {objects_ = o})

> set_creatures :: Creatures -> GS ()
> set_creatures c = modify_state (\s -> s {creatures_ = c})

> set_player :: Player -> GS ()
> set_player p = modify_state (\s -> s {player_ = p})

> set_player_location :: Pos -> GS ()
> set_player_location l = modify_state (\s -> s {player_location_ = l})

> set_line_of_sight :: LOS -> GS ()
> set_line_of_sight los = modify_state (\s -> s {line_of_sight_ = los})

> set_shortest_paths :: Pathing -> GS ()
> set_shortest_paths p = modify_state (\s -> s {shortest_paths_ = p})

> set_paused_switch :: Switch -> GS ()
> set_paused_switch p = modify_state (\s -> s {paused_switch_ = p})


> increment_depth :: GS ()
> increment_depth = get_dungeon_depth >>= set_dungeon_depth . (+1)



** Status line **

> status_line :: GS String
> status_line = get_ status_line_pure

> status_line_pure :: State -> String
> status_line_pure os =
>       let p = player_ os
>           h = health (cid_map (creatures_ os) IM.! (player_cid p))
>       in
>       "    > " ++ show (dungeon_depth_ os) ++
>       "    @ " ++ show (hp h) ++ "/" ++ show (max_hp h) ++
>       "    | " ++ show (xp_level p) ++ ":" ++ show (xp p) ++
>       "    ! " ++ show (num_potions (inventory p)) ++
>       "    ? " ++ show (num_scrolls (inventory p)) ++
>       "    ; " ++ show (hunger p)
