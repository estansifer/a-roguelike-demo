
> module State.Creature (
>       CID,
>       CreatureType(..),
>       MovementType(..),
>       Species(..),
>       Creature(..),
>       Creatures(),
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
>
> type CID = Int
>
> data CreatureType = Protagonist | Monster
>
> data MovementType =
>   Human |
>   WithHuman |
>   Timed |
>   TimedWave |
>   TimedAndWithHuman
>
> data Species = Species {
>       creature_type :: CreatureType,
>       movement_type :: [MovementType],
>       species_texture :: Char,
>       xp_reward :: Int,
>       max_damage :: Int,
>       max_health :: Int,
>       min_depth :: Int,
>       scarcity :: Int
>   }
>
> data Creature = Creature {
>       species :: Species,
>       health :: Health,
>       location :: Pos,
>       cid :: CID
>   }
>
> data Creatures = {
>       cids :: IntMap Creature,
>       locs :: Grid (MVar CID)
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
> empty_pos :: Creatures -> Pos -> IO Bool
> empty_pos cs p = isEmptyVar (locs cs IA.! p)
>
> creatures_list :: Creatures -> [Creature]
> creatures_list cs = IM.elems (cids cs)
>
> modify_creature :: CID -> (Creature -> Creature) -> Creatures -> Creatures
> modify_creature cid f cs = cs {cids = adjust f cid (cids cs)}
>
> update_creature_location :: CID -> Pos -> Creatures -> IO Creatures
> update_creature_location cid p_new cs = let p_old = location (cids cs ! cid) in do
>   var_old <- locs cs IA.! p_old
>   var_new <- locs cs IA.! p_new
>   tryTakeMVar var_old
>   tryPutMVar var_new cid
>   return $ modify_creature cid (\c -> c {location = p_new}) (cids cs)
>
> choose_path :: Pathing -> ValidDirs -> Creatures -> CID -> IO Dir
> choose_path paths vds cs cid = do
>   let p0 = location (cids cs ! cid)
>       d1 = snd $ paths IA.! p0
>       ds = d1 : sortBy (compare `on` (fst . (paths IA.!) . add_dir p0)) (vds IA.! p0)
>   es <- mapM (\d -> empty_pos cs (p0 `add_dir` d) || d == (0,0)) ds
>   let ds' = map fst $ filter snd $ zip ds es
>   case ds' of
>       (d:_) -> return d
>       [] -> return (0, 0)
