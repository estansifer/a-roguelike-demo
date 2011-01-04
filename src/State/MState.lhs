
> module State.MState (
>       initialize_game,
>       descend_level,
>
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
>       modify_creatures,
>       set_player,
>       set_location,
>       set_line_of_sight,
>       set_shortest_paths,
>       set_paused_switch,
>
>       modify_player,
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




** Constants **

> _starting_depth = 1



** Game initialization (impure) **

Assumes the game is currently uninitialized.

> initialize_game :: GS ()
> initialize_game = do
>   Uninitialized <- get_phase
>   params <- parameters
>
>   set_state $ State {dims_ = params}
>
>   set_random_terrain   -- This assumes that dims_ is well-defined
>   t <- terrain
>
>   set_depth _starting_depth
>
>   let bs = IA.bounds t
>   modify_state (\s -> s {_bounds = bs, _all_positions = Ix.range bs})
>
>   let vd = compute_valid_dirs t
>   set_valid_dirs vd
>
>   set_random_location
>   loc <- location
>
>   let los = compute_los t loc
>   set_kaart los
>   set_line_of_sight los
>
>   set_player new_player
>
>   seed <- random
>   set_objects (create_objects t seed)
>   set_shortest_paths (compute_shortest_paths vd loc)
>   set_phase Ongoing

> descend_level :: GS ()
> descend_level = do
>   increment_depth
>
>   set_random_terrain
>   t <- terrain
>
>   let vd = compute_valid_dirs t
>   set_valid_dirs vd
>
>   set_random_location
>   loc <- location
>
>   let los = compute_los t loc
>   set_kaart los
>   set_line_of_sight los
>
>   seed <- random
>   set_objects (create_objects t seed)
>   set_shortest_paths (compute_shortest_paths vd loc)


** Reading game state **

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



** Setting game state, low level **

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

> modify_creatures :: (Creatures -> Creatures) -> GS ()
> modify_creatures f = creatures_data >>= set_creatures_data . f

TODO -- there's more to it than just this

> set_location :: Pos -> GS ()
> set_location l = error "unimplemented" -- modify_state (\s -> s {player_location_ = l})

> set_line_of_sight :: LOS -> GS ()
> set_line_of_sight los = modify_state (\s -> s {line_of_sight_ = los})

> set_shortest_paths :: Pathing -> GS ()
> set_shortest_paths p = modify_state (\s -> s {shortest_paths_ = p})

> set_paused_switch :: Flag -> GS ()
> set_paused_switch p = modify_state (\s -> s {paused_switch_ = p})



> modify_player :: (Player -> Player) -> GS ()
> modify_player f = player >>= set_player . f

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






Because of laziness, the intervening levels are never computed out, the
only computation that takes place for the intervening levels is computing
the seeds for their random number generators.

 create_game_at_depth :: I -> I -> Int -> Int -> GameState
 create_game_at_depth w h s d =
   if d <= _starting_depth
       then create_game w h s
       else descend_level $ create_game_at_depth w h s (d - 1)


 is_legal_command :: GameState -> PlayerCommand -> Bool
 is_legal_command gs pc = case pc of
   Move dir -> dir `elem` (valid_dirs gs IA.! player_location gs)
   Drink -> False
   Read -> False
   Down -> Stairs `elem` (objects gs IA.! player_location gs)
   Quit -> False

 perform_command :: GameState -> PlayerCommand -> GameState
 perform_command gs pc = case pc of
   Move dir -> player_moves gs dir
   Drink -> error "Drinking potions is not implemented"
   Read -> error "Reading scrolls is not implemented"
   Down -> descend_level gs
   Quit -> error "'Quit' is not a legal movement"

 player_moves :: GameState -> Dir -> GameState
 player_moves gs dir = move_player_to gs (player_location gs `add_dir` dir)

Obsolete.

 move_player_to :: GameState -> Pos -> GameState
 move_player_to gs loc =
   let los' = compute_los (terrain gs) loc
       k' pos = (kaart gs IA.! pos) || (los' IA.! pos) in
   gs {
       kaart = arrayize k' (bounds gs),

       player_location = loc,
       line_of_sight = los',
       shortest_paths = compute_shortest_paths (valid_dirs gs) loc
   }

 can_move_in_direction :: GameState -> Dir -> Bool
 can_move_in_direction gs dir = dir `elem` (valid_dirs gs IA.! player_location gs)
