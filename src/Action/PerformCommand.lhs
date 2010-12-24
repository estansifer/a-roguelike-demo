
> module PerformCommand (
>       is_legal_command,
>       perform_command
>   ) where

> import Prelude hiding (read)

> import Data.STRef
> import Control.Monad.ST
> import qualified Data.Array.IArray as IA
> import Data.Ix
> import System.Random
>
> import BasicDefs
> import Util.Util (arrayize, random_from)
> import TerrainComputation
> import PlayerCommand
> import Player
> import GameState

> phase_door_range = 10

> is_legal_command :: GameState -> PlayerCommand -> Bool
> is_legal_command gs pc = (alive (player gs)) && case pc of
>   Move dir -> dir `elem` (valid_dirs gs IA.! player_location gs)
>   Drink -> has_potion (player gs)
>   Read -> has_scroll (player gs)
>   Down -> Stairs `elem` (objects gs IA.! player_location gs)
>   Quit -> False

> perform_command :: GameState -> PlayerCommand -> GameState
> perform_command gs pc = runST $ do
>   gs_var <- newSTRef gs
>   perf gs_var pc
>   readSTRef gs_var

> perf :: STRef s GameState -> PlayerCommand -> ST s ()
> perf gs_var pc = do
>   case pc of
>       Move dir -> move gs_var dir
>       Drink -> drink gs_var
>       Read -> read gs_var
>       Down -> down gs_var
>       Quit -> error "Quit is not a legal command"
>   age_player gs_var
>   age_environment gs_var

> move :: STRef s GameState -> Dir -> ST s ()
> move gs_var dir = do
>   gs <- readSTRef gs_var
>   change_location gs_var (player_location gs `add_dir` dir)
>   update_arrays gs_var
>   pick_up_objects gs_var

> change_location :: STRef s GameState -> Pos -> ST s ()
> change_location gs_var pos = modifySTRef gs_var (\gs -> gs {player_location = pos})

update line of sight, kaart, and shortest paths

> update_arrays :: STRef s GameState -> ST s ()
> update_arrays gs_var = do
>   gs <- readSTRef gs_var
>   let loc = player_location gs
>       los' = compute_los (terrain gs) loc
>       k' pos = (kaart gs IA.! pos) || (los' IA.! pos)
>   writeSTRef gs_var $ gs {
>           kaart = arrayize k' (bounds gs),
>           line_of_sight = los',
>           shortest_paths = compute_shortest_paths (valid_dirs gs) loc
>       }

> pick_up_objects :: STRef s GameState -> ST s ()
> pick_up_objects gs_var = do
>   gs <- readSTRef gs_var
>   let loc = player_location gs
>   let objs = filter (Stairs /=) (objects gs IA.! loc)
>   let stay_objs = filter (Stairs ==) (objects gs IA.! loc)
>   if null objs then return () else
>       writeSTRef gs_var $ gs {
>               player = pick_up_objs (player gs) objs,
>               objects = (objects gs) IA.// [(player_location gs, stay_objs)]
>           }

> drink :: STRef s GameState -> ST s ()
> drink gs_var = modifySTRef gs_var (\gs -> gs {player = drink_potion (player gs)})

> read :: STRef s GameState -> ST s ()
> read gs_var = modifySTRef gs_var (\gs -> gs {player = read_scroll (player gs)}) >>
>   phase_door gs_var

> phase_door :: STRef s GameState -> ST s ()
> phase_door gs_var = do
>   gs <- readSTRef gs_var
>   g_var <- newSTRef (gen gs)
>   dir <- phase_door' gs g_var
>   g <- readSTRef g_var
>   writeSTRef gs_var $ gs {
>           gen = g
>       }
>   move gs_var dir

> phase_door' :: RandomGen g => GameState -> STRef s g -> ST s Pos
> phase_door' gs g_var = do
>   dx <- random_from (-phase_door_range, phase_door_range) g_var
>   dy <- random_from (-phase_door_range, phase_door_range) g_var
>   let pos = player_location gs `add_dir` (dx, dy)
>   if inRange (bounds gs) pos && ((terrain gs IA.! pos) == Floor)
>       then return (dx, dy)
>       else phase_door' gs g_var

> down :: STRef s GameState -> ST s ()
> down gs_var = modifySTRef gs_var descend_level

> age_player :: STRef s GameState -> ST s ()
> age_player gs_var = modifySTRef gs_var (\gs -> gs {player = step_time (player gs)})

> age_environment :: STRef s GameState -> ST s ()
> age_environment gs_var = return ()
