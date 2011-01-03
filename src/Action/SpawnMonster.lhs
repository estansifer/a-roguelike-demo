
> module SpawnMonster (
>   ) where
>
> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM
>
> import Defs
> import Util.Util (repeat_while)
> import Util.Signal
> import State.State
> import State.MState
> import State.Creature


> unique_cid :: GS CID
> unique_cid = do
>   cs <- creatures_data
>   let cid = next_cid cs
>   set_creatures_data (cs {next_cid = (cid + 1)})
>   return cid

> spawn_monster :: Species -> GS ()
> spawn_monster species = do
>   los <- line_of_sight
>   t <- terrain
>   loc <- repeat_while (random_empty_location_m t) (return . (los IA.!))
>   cid <- unique_cid
>   hp <- full_health species
>   let creature = Creature {
>           species = species,
>           health = hp,
>           location = loc,
>           cid = cid
>       }
>   modify_creatures (IM.insert cid creature)
>   
