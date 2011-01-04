
> module State.Creature (
>       CID,
>       CreatureType(..),
>       MovementType(..),
>       Species(..),
>       Creature(..),
>       Creatures(),
>       full_health,
>       empty_pos,
>       creatures_list,
>       modify_creature,
>       update_creature_location,
>       choose_path
>   ) where
>
> import Data.IntMap (IntMap)
> import qualified Data.IntMap as IM
> import qualified Data.Array.IArray as IA
> import Data.Function (on)
>
> import Control.Concurrency.MVar
>
> import Defs
> import StupidClasses
> import State.Health
> import State.XP
> import State.Species
>
> type CID = Int
> data Creature = Creature {
>       species :: Species,
>       health :: Health,
>       location :: Pos
>   }
>
> data Creatures = {
>       cid_map :: IntMap Creature,
>       loc_map :: Grid (MVar CID),
>       next_cid :: CID
>   }
>
> instance Timeful Creature where
>   step c = c {health = step (health c)}
>
> instance Potionable Creature where
>   drink_poition c = c {health = drink_potion (health c)}
>
> instance Levelable Creature where
>   level_up c = c {health = level_up (heatlh c)}
>
> full_health :: Species -> Health
> full_health species = Health {
>       hp = max_health species,
>       max_hp = max_health species,
>       until_regen = 1
>   }
>
> creatures_list :: Creatures -> [Creature]
> creatures_list cs = IM.elems (c_map cs)
