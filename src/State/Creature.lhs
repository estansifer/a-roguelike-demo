
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
>       location :: Pos,
>       cid :: CID
>   }
>
> data Creatures = {
>       c_map :: IntMap Creature,
>       locs :: Grid (MVar CID),
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
> empty_pos :: Creatures -> Pos -> IO Bool
> empty_pos cs p = isEmptyVar (locs cs IA.! p)
>
> creatures_list :: Creatures -> [Creature]
> creatures_list cs = IM.elems (c_map cs)
>
> modify_creature :: CID -> (Creature -> Creature) -> Creatures -> Creatures
> modify_creature cid f cs = cs {c_map = adjust f cid (c_map cs)}
>
> update_creature_location :: CID -> Pos -> Creatures -> IO Creatures
> update_creature_location cid p_new cs = let p_old = location (c_map cs ! cid) in do
>   var_old <- locs cs IA.! p_old
>   var_new <- locs cs IA.! p_new
>   tryTakeMVar var_old
>   tryPutMVar var_new cid
>   return $ modify_creature cid (\c -> c {location = p_new}) (c_map cs)
>
> choose_path :: Pathing -> ValidDirs -> Creatures -> CID -> IO Dir
> choose_path paths vds cs cid = do
>   let p0 = location (c_map cs ! cid)
>       d1 = snd $ paths IA.! p0
>       ds = d1 : sortBy (compare `on` (fst . (paths IA.!) . add_dir p0)) (vds IA.! p0)
>   es <- mapM (\d -> empty_pos cs (p0 `add_dir` d) || d == (0,0)) ds
>   let ds' = map fst $ filter snd $ zip ds es
>   case ds' of
>       (d:_) -> return d
>       [] -> return (0, 0)
