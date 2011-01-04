
> module Action.Attack (
>   ) where

> damage_amt :: RandomM m => CID -> m Int
> damage_amt cid = do
>   creature <- get_creature cid
>   randomR (0, max_damage $ species creature)

> attack :: CID -> CID -> GS ()
> attack 
