
> module Action.Attack (
>       player_attack,
>       monster_attack
>   ) where

> import State.Health
> import State.Creature
> import State.State
> import State.MState

> damage_amt :: RandomM m => CID -> m Int
> damage_amt cid = do
>   creature <- get_creature cid
>   randomR (0, max_damage $ species creature)

> attack :: CID -> CID -> GS Bool
> attack cid1 cid2 = do
>   amount <- damage_amt cid1
>   deal_damage cid2 amount

> player_attack :: CID -> GS ()
> player_attack cid = do
>   pcid <- get_player_cid
>   killed <- attack pcid cid
>   -- TODO:  gain exp.
>   return ()

> monster_attack :: CID -> GS ()
> monster_attack cid = do
>   pcid <- get_player_cid
>   killed <- attack cid pcid
>   -- TODO:  end game if player killed
>   return ()
