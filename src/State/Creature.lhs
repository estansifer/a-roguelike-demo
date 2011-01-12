
> module State.Creature (
>       CID,
>       Creature(..),
>       Creatures(..),
>       register_kill_listener,
>       new_creature,
>       new_player_creature
>   ) where
>
> import Data.IntMap (IntMap)
> import qualified Data.IntMap as IM
> import qualified Data.Array.IArray as IA
> import Data.Function (on)
>
> import Control.Concurrent.MVar
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
>       location :: Pos,
>       killed :: Bool,
>       kill_listeners :: [IO ()]
>   }
>
> instance Show Creature where
>   show c = "$" ++ [species_texture (species c)] ++ (show $ location c) ++ (show $ killed c) ++ "%"
>
> data Creatures = Creatures {
>       cid_map :: IntMap Creature,
>       loc_map :: IOGrid (Maybe CID),
>       player_cid :: CID,
>       next_cid :: CID
>   }
>
> instance Timeful Creature where
>   step c = c {health = step (health c)}
>
> instance Potionable Creature where
>   drink_potion c = c {health = drink_potion (health c)}
>
> instance Levelable Creature where
>   level_up c = c {health = level_up (health c)}
>
> full_health :: Species -> Health
> full_health species = Health {
>       hp = max_health species,
>       max_hp = max_health species,
>       until_regen = 1
>   }
>
> register_kill_listener :: IO () -> Creature -> Creature
> register_kill_listener listener c = c{kill_listeners = listener:(kill_listeners c)}
>
> new_creature :: Species -> Pos -> Creature
> new_creature s l = Creature {
>       species = s,
>       health = full_health s,
>       location = l,
>       killed = False,
>       kill_listeners = []
>   }
>
> new_player_creature :: Creature
> new_player_creature = new_creature human undefined
