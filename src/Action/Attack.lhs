
> module Action.Attack (
>       player_attack,
>       monster_attack
>   ) where

> import Util.RandomM
> import Defs
> import State.Health
> import State.Species
> import State.Creature
> import State.State
> import Action.Creatures
> import Action.Player

> damage_amt :: CID -> U Integer
> damage_amt cid = do
>   creature <- get_creature cid
>   randomR (0, max_damage $ species creature)

> attack :: CID -> CID -> U Bool
> attack cid1 cid2 = damage_amt cid1 >>= deal_damage cid2

> player_attack :: CID -> U ()
> player_attack cid = do
>   pcid <- get_player_cid
>   killed <- attack pcid cid
>   -- TODO:  gain exp.
>   return ()

> monster_attack :: CID -> U ()
> monster_attack cid = do
>   pcid <- get_player_cid
>   attack cid pcid
>   -- TODO:  player death
>   return ()
