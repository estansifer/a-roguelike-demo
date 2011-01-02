
> module Action.PerformCommand (
>       perform_command_if_legal,
>       is_legal_command,
>       perform_command
>   ) where

> import Prelude hiding (read)

> import Control.Monad.ST
> import qualified Data.Array.IArray as IA
> import Ix
>
> import BasicDefs
> import Util.Util (arrayize)
> import Util.RandomM
> import TerrainComputation
> import PlayerCommand
> import State.Player
> import State.State
> import State.MState


** Constants **

> phase_door_range = 10




The operation 'perform_command_if_legal' is atomic;  it takes a global
lock before performing any actions.

> perform_command_if_legal :: PlayerCommand -> GS ()
> perform_command_if_legal !pc = lock $ do
>   legal <- is_legal_command pc
>   if legal then perform_command pc else return ()

** Is legal command? **

> is_legal_command :: PlayerCommand -> GS Bool
> is_legal_command pc = do
>   vd <- valid_dirs
>   loc <- location
>   p <- player
>   objs <- objects
>
>   return ((alive p) && (case pc of
>       Move dir -> dir `elem` (vd IA.! loc)
>       Drink -> has_potion p
>       Read -> has_scroll p
>       Down -> Stairs `elem` (objs IA.! loc)
>       Quit -> False))


** Modifying game state **

> perform_command :: PlayerCommand -> GS ()
> perform_command pc = do
>   case pc of
>       Move dir -> move dir
>       Drink -> drink
>       Read -> read
>       Down -> descend_level
>       Quit -> error "Quit is not a legal action"
>   move_normal_monsters
>   age_player
>   age_monsters

> move :: Dir -> GS ()
> move dir = do
>   loc <- location
>   set_location (loc `add_dir` dir)
>   update_arrays
>   pick_up_objects

update line of sight, kaart, and shortest paths

> update_arrays :: GS ()
> update_arrays = do
>   bs <- bounds
>   t <- terrain
>   vd <- valid_dirs
>   k <- kaart
>   loc <- location
>
>   let new_los = compute_los t loc
>       new_k pos = (k IA.! pos) || (new_los IA.! pos)
>
>   set_kaart $ arrayize new_k bs
>   set_line_of_sight new_los
>   set_shortest_paths (compute_shortest_paths vd loc)

> pick_up_objects :: GS ()
> pick_up_objects = do
>   objs <- objects
>   loc <- location
>
>   let remove = filter (Stairs /=) (objs IA.! loc)
>   let stay   = filter (Stairs ==) (objs IA.! loc)
>   if null remove then return () else do
>       p <- player
>       set_player $ pick_up_objs p remove
>       set_objects $ objs IA.// [(loc, stay)]


> drink :: GS ()
> drink = modify_player drink_potion 

> read :: GS ()
> read = modify_player read_scroll >> phase_door

> phase_door :: GS ()
> phase_door = do
>   bs <- bounds
>   t <- terrain
>   loc <- location
>
>   dx <- randomR (-phase_door_range, phase_door_range)
>   dy <- randomR (-phase_door_range, phase_door_range)
>   let pos = loc `add_dir` (dx, dy)
>   if inRange bs pos && ((t IA.! pos) == Floor)
>       then move (dx, dy)
>       else phase_door

> age_player :: GS ()
> age_player = modify_player step_time

> move_normal_monsters :: GS ()
> move_normal_monsters = return ()

> age_monsters :: GS ()
> age_monsters = return ()
