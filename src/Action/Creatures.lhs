
> module Action.Creatures (
>       unique_cid,
>
>       get_player_cid,
>       get_player_creature,
>       get_player_location,
>       modify_player_creature,
>
>       get_cid,
>       get_creature,
>       get_cid_at,
>       get_creatures_list,
>       get_living_creatures,
>       get_cids_by_movement,
>
>       is_empty_pos,
>       modify_creature,
>       create_creature_at,
>       move_creature,
>       choose_path,
>       phase_door,
>       deal_damage,
>       creature_drink,
>       age_creature
>   ) where

> import qualified Data.Array.IArray as IA
> import Data.Array.MArray (readArray, writeArray)
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

> unique_cid :: U CID
> unique_cid = do
>   creatures <- get_creatures
>   let cid = next_cid creatures
>   set_creatures (creatures {next_cid = (cid + 1)})
>   return cid


> get_player_cid :: U CID
> get_player_cid = fmap player_cid get_creatures

> get_player_creature :: U Creature
> get_player_creature = get_player_cid >>= get_creature

> get_player_location :: U Pos
> get_player_location = fmap location get_player_creature

> modify_player_creature :: (Creature -> Creature) -> U ()
> modify_player_creature f =
>   modify_creatures $ \creatures -> creatures {
>       cid_map = IM.adjust f (player_cid creatures) (cid_map creatures)
>   }


> get_cid :: Creature -> U CID
> get_cid creature = do
>   creatures <- get_creatures
>   mcid <- liftIO $ readArray (loc_map creatures) (location creature)
>   case mcid of
>       Just cid -> return cid
>       Nothing -> db (show creature) >> error "what"

> get_creature :: CID -> U Creature
> get_creature cid = do
>   creatures <- get_creatures
>   return (cid_map creatures IM.! cid)

> get_cid_at :: Pos -> U (Maybe CID)
> get_cid_at pos = do
>   creatures <- get_creatures
>   liftIO $ readArray (loc_map creatures) pos

> get_creatures_list :: U [Creature]
> get_creatures_list = do
>   creatures <- get_creatures
>   return $ IM.elems $ cid_map creatures

> get_living_creatures :: U [Creature]
> get_living_creatures = fmap (filter (not . killed)) get_creatures_list

> get_cids_by_movement :: MovementType -> U [CID]
> get_cids_by_movement mt = do
>   creatures <- get_creatures
>   return $ map fst $
>       filter (elem mt . movement_type . species . snd) $
>       filter (not . killed . snd) $
>               IM.assocs $ cid_map creatures



> is_empty_pos :: Pos -> U Bool
> is_empty_pos pos = do
>   creatures <- get_creatures
>   mcid <- liftIO $ readArray (loc_map creatures) pos
>   return $ case mcid of {Nothing -> True; _ -> False}

> modify_creature :: CID -> (Creature -> Creature) -> U ()
> modify_creature cid f = modify_creatures $ \creatures -> creatures {
>       cid_map = IM.adjust f cid (cid_map creatures)
>   }

> create_creature_at :: Species -> Pos -> U CID
> create_creature_at sp pos = do
>   cid <- unique_cid   -- This must come before the 'get_creatures'
>   creatures <- get_creatures
>   set_creatures $ creatures {cid_map = 
>           IM.insert cid (new_creature sp pos) (cid_map creatures)
>       }
>   liftIO $ writeArray (loc_map creatures) pos $ Just cid
>   return cid

> move_creature :: CID -> Pos -> U ()
> move_creature cid p_new = do
>   creatures <- get_creatures
>   let p_old = location (cid_map creatures IM.! cid)
>   liftIO $ writeArray (loc_map creatures) p_old Nothing
>   liftIO $ writeArray (loc_map creatures) p_new (Just cid)
>
>   modify_creature cid (\c -> c {location = p_new})

> choose_path :: CID -> U Dir
> choose_path cid = do
>   pathing <- get_shortest_paths
>   valid_dirs <- get_valid_dirs
>   pos <- fmap location $ get_creature cid
>   let dir0 = snd $ pathing IA.! pos
>   let closest = compare `on` (fst . (pathing IA.!) . add_dir pos)
>   let dirs = [dir0] ++
>           (sortBy closest (filter (/= (0, 0)) (valid_dirs IA.! pos))) ++
>           [(0, 0)]
>   emptys <- forM dirs $ \dir ->
>       fmap (|| (dir == (0, 0))) (is_empty_pos (pos `add_dir` dir))
>   let dirs' = map fst $ filter snd $ zip dirs emptys
>   return $ head $ dirs'

> phase_door :: CID -> U ()
> phase_door cid = do
>   creatures <- get_creatures
>   bounds <- asks (bounds . constants)
>   terrain <- get_terrain
>   let pos = location (cid_map creatures IM.! cid)
>   new_pos <- repeat_until
>           (do
>               dx <- randomR (-phase_door_range, phase_door_range)
>               dy <- randomR (-phase_door_range, phase_door_range)
>               return (pos `add_dir` (dx, dy)))
>           (\p ->
>               if not $ Ix.inRange bounds p then return False else do
>               e <- is_empty_pos p
>               return ((e || (p == pos)) && (terrain IA.! p) == Floor))
>   move_creature cid new_pos

> deal_damage :: CID -> Integer -> U Bool
> deal_damage cid amount = do
>   creatures <- get_creatures
>   let c = cid_map creatures IM.! cid
>   if (killed c) then return False else do
>       let c' = c {health = take_damage amount (health c)}
>       if hp (health c') < 0
>           then do
>               liftIO $ sequence $ kill_listeners c'
>               modify_creature cid (const (c'{killed = True, kill_listeners = []}))
>               liftIO $ writeArray (loc_map creatures) (location c) Nothing
>               return True
>           else do
>               modify_creature cid (const c')
>               return False

> creature_drink :: CID -> U ()
> creature_drink cid = modify_creature cid drink_potion

> age_creature :: CID -> U ()
> age_creature cid = modify_creature cid step
