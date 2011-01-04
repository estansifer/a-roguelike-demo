
> module Action.Creatures (
>       unique_cid,
>       get_creature,
>       get_cid_at,
>       get_creatures_list,
>       is_empty_pos,
>       new_creatures, reset_creatures,
>       create_creature_at,
>       update_creature_location,
>       choose_path
>   ) where

> import qualified Data.Array.IArray as IA
> import qualified Data.IntMap as IM

> import Util.Util (arrayizeM)
> import State.Species
> import State.Creature
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
>   mcid <- tryTakeMVar mvar
>   case mcid of
>       Just cid -> putMVar mvar cid >> return mcid
>       Nothing -> return mcid

> get_creatures_list :: GS [Creature]
> get_creatures_list = do
>   creatures <- get_creatures
>   return (IM.elems (cid_map creatures))

> is_empty_pos :: Pos -> GS Bool
> is_empty_pos pos = do
>   creatures <- get_creatures
>   isEmptyVar (loc_map creatures IA.! pos)

> new_creatures :: GS ()
> new_creatures = do
>   bounds <- get_bounds
>   l_map <- arrayizeM newEmptyMVar bounds
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
>
>   bounds <- get_bounds
>   l_map <- arrayizeM newEmptyMVar bounds
>   putMVar (l_map IA.! loc) cid
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
> create_player = create_creature_at Human (1, 1)

> create_creature_at :: Species -> Pos -> GS CID
> create_creature_at sp pos = do
>   cid <- unique_cid
>   modify_creatures $ IM.insert cid $ Creature {
>           species = sp,
>           health = full_health sp,
>           location = pos
>       }
>   creatures <- get_creatures
>   putMVar (loc_map creatures IA.! pos) cid
>   return cid

> update_creature_location :: CID -> Pos -> GS ()
> update_creature_location cid p_new = do
>   creatures <- get_creatures
>   let p_old = location (cid_map creatures ! cid)
>   var_old <- loc_map creatures IA.! p_old
>   var_new <- loc_map creatures IA.! p_new
>   tryTakeMVar var_old
>   tryPutMVar var_new cid
>   modify_creature cid (\c -> c {location = p_new})

> choose_path :: CID -> GS Dir
> choose_path cid = do
>   pathing <- get_shortest_paths
>   valid_dirs <- get_valid_dirs
>   creatures <- get_creatures
>   let pos = location (cid_map creatures IM.! cid)
>   let dir0 = snd $ pathing IA.! pos
>   let closest = compare `on` (fst . (pathing IA.!) . add_dir pos)
>   let dirs = dir0 : sortBy closest (valid_dirs IA.! pos)
>   emptys <- mapM (\dir -> is_empty_pos (pos `add_dir` dir) || d == (0, 0)) dirs
>   let dirs' = map fst $ filter snd $ zip dirs emptys
>   case dirs' of
>       (dir:_) -> return dir
>       [] -> return (0, 0)     -- should never happen, as (0, 0) is a valid dir