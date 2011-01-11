
> module Action.Monster (
>       perform_monster_action_if_alive
>   ) where

> import Control.Monad (unless, when)
> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM

> import Util.Util (db)
> import Defs
> import Constants
> import State.Creature
> import State.State
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

> perform_monster_action_if_alive :: CID -> U ()
> perform_monster_action_if_alive cid = do
>   creatures <- get_creatures
>   case IM.lookup cid (cid_map creatures) of
>       Nothing -> return ()
>       Just creature -> unless (killed creature) $ do
>           let pos = location creature
>           p_pos <- get_player_location
>           let dir_to_player = p_pos `sub_pos` pos
>
>           valid_dirs <- get_valid_dirs
>           let can_attack = dir_to_player `elem` (valid_dirs IA.! pos)
>
>           if can_attack
>               then monster_attack cid
>               else when
>                       (norm dir_to_player <= smelling_range_squared)
>                       (move_towards_player cid pos)
>           age_creature cid

> move_towards_player :: CID -> Pos -> U ()
> move_towards_player cid pos = choose_path cid >>= move_creature cid . (add_dir pos)
