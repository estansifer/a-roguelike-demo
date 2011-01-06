
> module Action.Creatures (
>       unique_cid,
>       get_creature,
>       get_cid_at,
>       get_creatures_list,
>       get_living_creatures,
>       get_cids_by_movement,
>       is_empty_pos,
>       new_creatures, reset_creatures,
>       modify_creature,
>       create_player, create_creature_at,
>       update_creature_location,
>       choose_path,
>       phase_door,
>       deal_damage,
>       creature_drink,
>       age_creature
>   ) where

> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM
> import qualified Data.Ix as Ix

> import Data.Function (on)
> import Data.List (sortBy)
> import Control.Concurrent
> import Control.Monad (forM)

> import Util.Util (arrayizeM, repeat_until, db)
> import Util.RandomM
> import Constants
> import Defs
> import StupidClasses
> import State.Species
> import State.Health
> import State.Creature
> import State.Player
> import State.State
> import State.MState

> unique_cid :: GS CID
> unique_cid = do
>   creatures <- get_creatures
>   let cid = next_cid creatures
>   set_creatures (creatures {next_cid = (cid + 1)})
>   return cid

> get_creature :: CID -> GS Creature
> get_creature cid = fmap (\creatures -> cid_map creatures IM.! cid) get_creatures

> get_cid_at :: Pos -> GS (Maybe CID)
> get_cid_at pos = do
>   creatures <- get_creatures
>   let mvar = loc_map creatures IA.! pos
>   m_cid <- liftIO $ tryTakeMVar mvar
>   case m_cid of
>       Just cid -> liftIO $ putMVar mvar cid >> return m_cid
>       Nothing -> return m_cid

> get_creatures_list :: GS [Creature]
> get_creatures_list = do
>   creatures <- get_creatures
>   return (IM.elems (cid_map creatures))

> get_living_creatures :: GS [Creature]
> get_living_creatures = fmap (filter (not . killed)) get_creatures_list

> get_cids_by_movement :: MovementType -> GS [CID]
> get_cids_by_movement mt = do
>   creatures <- get_creatures
>   return $ map fst $
>       filter (elem mt . movement_type . species . snd) $
>       filter (not . killed . snd) $
>               IM.assocs $ cid_map creatures

> is_empty_pos :: Pos -> GS Bool
> is_empty_pos pos = do
>   creatures <- get_creatures
>   liftIO $ isEmptyMVar (loc_map creatures IA.! pos)

> new_creatures :: GS ()
> new_creatures = do
>   bounds <- get_bounds
>   l_map <- liftIO $ arrayizeM (const newEmptyMVar) bounds
>   set_creatures $ Creatures {
>           cid_map = IM.empty,
>           loc_map = l_map,
>           next_cid = 1
>       }

> reset_creatures :: GS ()
> reset_creatures = do
>   player <- get_player
>   let cid = player_cid player
>   old_creatures <- get_creatures
>   loc <- get_player_location

We are using the fact that the player has no kill listeners registered on it.

>   liftIO $ sequence
>       [k | c <- IM.elems (cid_map old_creatures), k <- kill_listeners c]
>
>   bounds <- get_bounds
>   l_map <- liftIO $ arrayizeM (const newEmptyMVar) bounds
>   liftIO $ putMVar (l_map IA.! loc) cid
>   set_creatures $ Creatures {
>           cid_map = IM.singleton cid (cid_map old_creatures IM.! cid),
>           loc_map = l_map,
>           next_cid = cid + 1
>       }

> modify_creatures :: (Creatures -> Creatures) -> GS ()
> modify_creatures f = modify_state $ \s -> s {
>       creatures_ = f $ creatures_ s
>   }

> modify_creature :: CID -> (Creature -> Creature) -> GS ()
> modify_creature cid f = modify_creatures $ \creatures -> creatures {
>       cid_map = IM.adjust f cid (cid_map creatures)
>   }

> create_player :: GS CID
> create_player = set_player_location (1, 1) >> create_creature_at human (1, 1)

> create_creature_at :: Species -> Pos -> GS CID
> create_creature_at sp pos = do
>   cid <- unique_cid
>   creatures <- get_creatures
>   set_creatures $ creatures {cid_map = 
>           IM.insert cid (Creature {
>               species = sp,
>               health = full_health sp,
>               location = pos,
>               killed = False,
>               kill_listeners = []
>           }) (cid_map creatures)
>       }
>   liftIO $ putMVar (loc_map creatures IA.! pos) cid
>   return cid

> update_creature_location :: CID -> Pos -> GS ()
> update_creature_location cid p_new = do
>   creatures <- get_creatures
>   let p_old = location (cid_map creatures IM.! cid)
>   let var_old = loc_map creatures IA.! p_old
>   let var_new = loc_map creatures IA.! p_new
>   liftIO $ tryTakeMVar var_old
>   liftIO $ tryPutMVar var_new cid
>   modify_creature cid (\c -> c {location = p_new})

> choose_path :: CID -> GS Dir
> choose_path cid = do
>   pathing <- get_shortest_paths
>   valid_dirs <- get_valid_dirs
>   pos <- fmap location $ get_creature cid
>   let dir0 = snd $ pathing IA.! pos
>   let closest = compare `on` (fst . (pathing IA.!) . add_dir pos)
>   let dirs = dir0 : sortBy closest (valid_dirs IA.! pos)
>   emptys <- forM dirs $ \dir ->
>       fmap (|| (dir == (0, 0))) (is_empty_pos (pos `add_dir` dir))
>   let dirs' = map fst $ filter snd $ zip dirs emptys
>   case dirs' of
>       (dir:_) -> return dir
>       [] -> return (0, 0)     -- should never happen, as (0, 0) is a valid dir

> phase_door :: CID -> GS ()
> phase_door cid = do
>   creatures <- get_creatures
>   bounds <- get_bounds
>   terrain <- get_terrain
>   let pos = location (cid_map creatures IM.! cid)
>   new_pos <- repeat_until
>           (do
>               dx <- randomR (-phase_door_range, phase_door_range)
>               dy <- randomR (-phase_door_range, phase_door_range)
>               return (pos `add_dir` (dx, dy)))
>           (\p -> do
>               e <- is_empty_pos p
>               return (e && Ix.inRange bounds p && (terrain IA.! p) == Floor))
>   update_creature_location cid new_pos

> deal_damage :: CID -> Integer -> GS Bool
> deal_damage cid amount = do
>   creatures <- get_creatures
>   let c = cid_map creatures IM.! cid
>   if (killed c) then return False else do
>       let c' = c {health = take_damage amount (health c)}
>       if hp (health c') < 0
>           then do
>               liftIO $ sequence $ kill_listeners c'
>               modify_creature cid (const (c'{killed = True}))
>               liftIO $ tryTakeMVar $ loc_map creatures IA.! (location c)
>               return True
>           else do
>               modify_creature cid (const c')
>               return False

> creature_drink :: CID -> GS ()
> creature_drink cid = modify_creature cid drink_potion

> age_creature :: CID -> GS ()
> age_creature cid = modify_creature cid step
