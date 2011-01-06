
> module Action.Monster (
>       perform_monster_action
>   ) where

> import qualified Data.Array.IArray as IA

> import Defs
> import Constants
> import State.Creature
> import State.State
> import State.MState
> import Action.Creatures
> import Action.Attack

Monster behavior:

If player is adjacent, attack player.

If player is within line of sight or smelling range, move in shortest
path towards player.

Otherwise, sit still.


perform_monster_action must check that the monster is still in the cid_map
and is still alive, as this code can be reached after the monster has been
killed.

> perform_monster_action :: CID -> GS ()
> perform_monster_action cid = do
>   pos <- fmap location $ get_creature cid
>   p_pos <- get_player_location
>   let dir_to_player = p_pos `sub_pos` pos
>   valid_dirs <- get_valid_dirs
>   let can_attack = dir_to_player `elem` (valid_dirs IA.! pos)
>
>   if can_attack then monster_attack cid else do
>       if norm dir_to_player <= smelling_range_squared
>           then move_towards_player cid
>           else return ()
>   age_creature cid

> move_towards_player :: CID -> GS ()
> move_towards_player cid = choose_path cid >>= update_creature_location cid
