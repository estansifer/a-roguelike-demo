
> module GameState (
>       GameState (
>           width,
>           height,
>           bounds,
>           range,
>           depth,
>           gen,
>           terrain,
>           valid_dirs,
>           kaart,
>           objects,
>           player,
>           player_location,
>           line_of_sight,
>           shortest_paths),
>       create_game,
>       create_game_at_depth,
>       descend_level,
>       status_line,
>       move_player_to,
>       can_move_in_direction,
>   ) where

>
> import qualified Data.Array.IArray as IA
> import System.Random
> import qualified Ix
>
> import BasicDefs
> import Util.Util (arrayize, more_seeds)
> import CreateTerrain
> import CreateObjects
> import TerrainComputation
> import PlayerCommand
> import Player

> _starting_depth = 1

> data GameState = GameState {
>       width :: I,
>       height :: I,
>       bounds :: (Pos, Pos),
>       range :: [Pos],
>
>       depth :: Int,
>
>       gen :: StdGen,
>
>       terrain :: Terrain,
>       valid_dirs :: ValidDirs,
>
>       kaart :: Kaart,
>
>       objects :: Objects,
>
>       -- something for the monsters
>
>       {- the following are closely tied to player state -}
>       player :: Player,
>       player_location :: Pos,
>       line_of_sight :: LOS,
>       shortest_paths :: Pathing
>   }

> create_game :: I -> I -> Int -> GameState
> create_game w h s =
>   let (s1:s2:s3:s4:_) = more_seeds s
>       t = create_terrain w h s1
>       vd = compute_valid_dirs t
>       loc = random_empty_location t s2
>       los = compute_los t loc
>   in
>   GameState {
>       width = w,
>       height = h,
>       bounds = IA.bounds t,
>       range = Ix.range (IA.bounds t),
>
>       depth = _starting_depth,
>
>       gen = mkStdGen s3,
>
>       terrain = t,
>       valid_dirs = vd,
>
>       kaart = los,
>
>       objects = create_objects t s4,
>
>       player = new_player,
>       player_location = loc,
>       line_of_sight = los,
>       shortest_paths = compute_shortest_paths vd loc
>   }

Because of laziness, the intervening levels are never computed out, the
only computation that takes place for the intervening levels is computing
the seeds for their random number generators.

> create_game_at_depth :: I -> I -> Int -> Int -> GameState
> create_game_at_depth w h s d =
>   if d <= _starting_depth
>       then create_game w h s
>       else descend_level $ create_game_at_depth w h s (d - 1)

> descend_level :: GameState -> GameState
> descend_level gs =
>   let (s1, g1) = random (gen gs)
>       (s2, g2) = random g1
>       (s3, g3) = random g2
>       t = create_terrain (width gs) (height gs) s1
>       vd = compute_valid_dirs t
>       loc = random_empty_location t s2
>       los = compute_los t loc
>   in
>   gs {
>       depth = depth gs + 1,
>       
>       gen = g3,
>
>       terrain = t,
>       valid_dirs = vd,
>
>       kaart = los,
>
>       objects = create_objects t s3,
>
>       player_location = loc,
>       line_of_sight = los,
>       shortest_paths = compute_shortest_paths vd loc
>   }


> status_line :: GameState -> String
> status_line gs = let p = player gs in
>       "    > " ++ show (depth gs) ++
>       "    @ " ++ show (health p) ++ "/" ++ show (max_health p) ++
>       "    | " ++ show (xp_level p) ++ ":" ++ show (xp p) ++
>       "    ! " ++ show (num_potions (inventory p)) ++
>       "    ? " ++ show (num_scrolls (inventory p)) ++
>       "    ; " ++ show (hunger p)

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

> move_player_to :: GameState -> Pos -> GameState
> move_player_to gs loc =
>   let los' = compute_los (terrain gs) loc
>       k' pos = (kaart gs IA.! pos) || (los' IA.! pos) in
>   gs {
>       kaart = arrayize k' (bounds gs),
>
>       player_location = loc,
>       line_of_sight = los',
>       shortest_paths = compute_shortest_paths (valid_dirs gs) loc
>   }

> can_move_in_direction :: GameState -> Dir -> Bool
> can_move_in_direction gs dir = dir `elem` (valid_dirs gs IA.! player_location gs)
