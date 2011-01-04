
> module Action.Monster (
>       perform_monster_action
>   ) where

Monster behavior:

If player is adjacent, attack player.

If player is within line of sight or smelling range, move in shortest
path towards player.


> perform_monster_action :: CID -> GS ()
> perform_monster_action cid = do
