
> {-# LANGUAGE TypeSynonymInstances #-}
>
> module State.GameState (
>       GameParameters,
>       GameState(..),
>       GS, liftIO, 
>       run_game,
>       initialize_game,
>       descend_level,
>
>       dims,
>       bounds,
>       all_positions,
>       dungeon_depth,
>       terrain,
>       valid_dirs,
>       kaart,
>       objects,
>       player,
>       location,
>       line_of_sight,
>       shortest_paths,
>
>       set_kaart,
>       set_objects,
>       set_player,
>       set_location,
>       set_line_of_sight,
>       set_shortest_paths,
>
>       modify_player,
>
>       status_line
>   ) where

> import Data.IORef
> import qualified Data.Array.IArray as IA
> import qualified Ix
> import System.Random (randomIO, randomRIO)
>
> -- from the transformers library
> import qualified Control.Monad.Trans as CMT (liftIO)
> import Control.Monad.Trans.Reader
>
> import BasicDefs
> import Util.Util (arrayize, more_seeds)
> import Util.RandomM
> import Create.CreateTerrain
> import Create.CreateObjects
> import TerrainComputation
> import PlayerCommand
> import State.Player




** Constants **

> _starting_depth = 1




** Game parameters (pure) **

> type GameParameters = (I, I)





** Game creation (pure) **

In the future, we will have a notion of an "empty" or "uncreated" game state,
containing only the parameters, which can be explicitly expanded into a starting
state.  That way the seed will not have to be passed into this function.

> pure_new_game :: GameParameters -> GameState
> pure_new_game gp = Uninitialized gp




** Global game state (pure) **

We don't export 'OngoingState' so as not to expose the internal state of the
game while it is inprogress.  We do export 'Uninitialized', 'Ongoing', 'Over',
and 'EndCondition' so that the end game state is exposed.

> data GameState =
>       Uninitialized GameParameters |
>       Ongoing OngoingState
>       -- Over EndCondition

> data OngoingState = OngoingState {
>       _dims :: Pos,
>       _bounds :: (Pos, Pos),
>       _all_positions :: [Pos],
>
>       _dungeon_depth :: Int,
>

We would need to include a random number generator to gain pure behavior,
but because everything is taking place in the GS monad, we can just use
the IO standard random number generator.

>       -- _gen :: StdGen,
>
>       _terrain :: Terrain,
>       _valid_dirs :: ValidDirs,
>
>       _kaart :: Kaart,
>
>       _objects :: Objects,
>
>       -- something for monsters
>

The remaining fields are intimately linked to the player state, and must
be updated on every movement of the player.  Some of the arrays could
be replaced with IO arrays for greater efficiency.

>       _player :: Player,
>       _player_location :: Pos,
>       _line_of_sight :: LOS,
>       _shortest_paths :: Pathing
>   }

> ongoing :: GameState -> OngoingState
> ongoing (Ongoing os) = os




** The GS monad **


A value of type 'GS a' means a value of type 'a' that possibly uses or modifies
the global game state, and possibly uses actions from the IO monad.

We receive both

    Monad GS
    MonadIO GS

from the Control.Monad.Trans.Reader module.  In particular we have the following
operation:

liftIO :: IO a -> GS a

(which is identical to 'lift', so it is just a choice of convention for which
name we use).  Thus we can perform IO actions in the GS monad.

> type GS = ReaderT (IORef GameState) IO

> liftIO :: IO a -> GS a
> liftIO = CMT.liftIO

> instance RandomM GS where
>   random = liftIO randomIO
>   randomR r = liftIO $ randomRIO r

The 'run_game' function has to create the initial game state, so in particular
it requires the game parameters.  The initial game state is 'Uninitialized'.

> run_game :: GameParameters -> GS a -> IO (a, GameState)
> run_game gp io_x = do
>   game_var <- newIORef $ pure_new_game gp
>   x <- runReaderT io_x game_var
>   end_game_state <- readIORef game_var
>   return (x, end_game_state)

> get_state :: GS GameState
> get_state = do
>   gs_var <- ask
>   gs <- liftIO $ readIORef gs_var
>   return gs

> set_state :: GameState -> GS ()
> set_state gs = do
>   gs_var <- ask
>   liftIO $ writeIORef gs_var gs

> modify_state :: (GameState -> GameState) -> GS ()
> modify_state f = do
>   gs_var <- ask
>   liftIO $ modifyIORef gs_var f




** Game initialization (impure) **

Assumes the game is currently uninitialized.

> initialize_game :: GS ()
> initialize_game = do
>   Uninitialized gp <- get_state
>   set_state $ Ongoing $ OngoingState {_dims = gp}
>
>   set_depth _starting_depth
>
>   set_random_terrain   -- This assumes that _dims is well-defined
>   t <- terrain
>
>   let bs = IA.bounds t
>   modify (\os -> os {_bounds = bs, _all_positions = Ix.range bs})
>
>   let vd = compute_valid_dirs t
>   set_valid_dirs vd
>
>   set_random_location
>   loc <- location
>
>   let los = compute_los t loc
>   set_kaart los
>   set_line_of_sight los
>
>   set_player new_player
>
>   seed <- random
>   set_objects (create_objects t seed)
>   set_shortest_paths (compute_shortest_paths vd loc)

> descend_level :: GS ()
> descend_level = do
>   increment_depth
>
>   set_random_terrain
>   t <- terrain
>
>   let vd = compute_valid_dirs t
>   set_valid_dirs vd
>
>   set_random_location
>   loc <- location
>
>   let los = compute_los t loc
>   set_kaart los
>   set_line_of_sight los
>
>   seed <- random
>   set_objects (create_objects t seed)
>   set_shortest_paths (compute_shortest_paths vd loc)


** Reading game state **

> get :: (OngoingState -> a) -> GS a
> get f = fmap (f . ongoing) get_state


> dims :: GS Pos
> dims = get _dims

> bounds :: GS (Pos, Pos)
> bounds = get _bounds

> all_positions :: GS [Pos]
> all_positions = get _all_positions

> dungeon_depth :: GS Int
> dungeon_depth = get _dungeon_depth

> terrain :: GS Terrain
> terrain = get _terrain

> valid_dirs :: GS ValidDirs
> valid_dirs = get _valid_dirs

> objects :: GS Objects
> objects = get _objects

> kaart :: GS Kaart
> kaart = get _kaart

> player :: GS Player
> player = get _player

> location :: GS Pos
> location = get _player_location

> line_of_sight :: GS LOS
> line_of_sight = get _line_of_sight

> shortest_paths :: GS Pathing
> shortest_paths = get _shortest_paths


> is_alive :: GS Bool
> is_alive = get (alive . _player)



** Setting game state, low level **

> modify :: (OngoingState -> OngoingState) -> GS ()
> modify f = modify_state (Ongoing . f . ongoing)

> set_depth :: Int -> GS ()
> set_depth n = modify (\os -> os {_dungeon_depth = n})

> set_terrain :: Terrain -> GS ()
> set_terrain t = modify (\os -> os {_terrain = t})

> set_valid_dirs :: ValidDirs -> GS ()
> set_valid_dirs vd = modify (\os -> os {_valid_dirs = vd})

> set_kaart :: Kaart -> GS ()
> set_kaart k = modify (\os -> os {_kaart = k})


> set_objects :: Objects -> GS ()
> set_objects o = modify (\os -> os {_objects = o})

> set_player :: Player -> GS ()
> set_player p = modify (\os -> os {_player = p})

> set_location :: Pos -> GS ()
> set_location l = modify (\os -> os {_player_location = l})

> set_line_of_sight :: LOS -> GS ()
> set_line_of_sight los = modify (\os -> os {_line_of_sight = los})

> set_shortest_paths :: Pathing -> GS ()
> set_shortest_paths p = modify (\os -> os {_shortest_paths = p})



> modify_player :: (Player -> Player) -> GS ()
> modify_player f = player >>= set_player . f

> increment_depth :: GS ()
> increment_depth = dungeon_depth >>= set_depth . (+1)

> set_random_terrain :: GS ()
> set_random_terrain = do
>   s <- random
>   d <- dims
>   set_terrain (create_terrain d s)

> set_random_location :: GS ()
> set_random_location = (terrain >>= random_empty_location_m) >>= set_location




** Status line **

> status_line :: GS String
> status_line = get status_line_pure

> status_line_pure :: OngoingState -> String
> status_line_pure os = let p = _player os in
>       "    > " ++ show (_dungeon_depth os) ++
>       "    @ " ++ show (health p) ++ "/" ++ show (max_health p) ++
>       "    | " ++ show (xp_level p) ++ ":" ++ show (xp p) ++
>       "    ! " ++ show (num_potions (inventory p)) ++
>       "    ? " ++ show (num_scrolls (inventory p)) ++
>       "    ; " ++ show (hunger p)






Because of laziness, the intervening levels are never computed out, the
only computation that takes place for the intervening levels is computing
the seeds for their random number generators.

 create_game_at_depth :: I -> I -> Int -> Int -> GameState
 create_game_at_depth w h s d =
   if d <= _starting_depth
       then create_game w h s
       else descend_level $ create_game_at_depth w h s (d - 1)


 is_legal_command :: GameState -> PlayerCommand -> Bool
 is_legal_command gs pc = case pc of
   Move dir -> dir `elem` (valid_dirs gs IA.! player_location gs)
   Drink -> False
   Read -> False
   Down -> Stairs `elem` (objects gs IA.! player_location gs)
   Quit -> False

 perform_command :: GameState -> PlayerCommand -> GameState
 perform_command gs pc = case pc of
   Move dir -> player_moves gs dir
   Drink -> error "Drinking potions is not implemented"
   Read -> error "Reading scrolls is not implemented"
   Down -> descend_level gs
   Quit -> error "'Quit' is not a legal movement"

 player_moves :: GameState -> Dir -> GameState
 player_moves gs dir = move_player_to gs (player_location gs `add_dir` dir)

Obsolete.

 move_player_to :: GameState -> Pos -> GameState
 move_player_to gs loc =
   let los' = compute_los (terrain gs) loc
       k' pos = (kaart gs IA.! pos) || (los' IA.! pos) in
   gs {
       kaart = arrayize k' (bounds gs),

       player_location = loc,
       line_of_sight = los',
       shortest_paths = compute_shortest_paths (valid_dirs gs) loc
   }

 can_move_in_direction :: GameState -> Dir -> Bool
 can_move_in_direction gs dir = dir `elem` (valid_dirs gs IA.! player_location gs)
