
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE Rank2Types #-}
>
> module State.State (
>       Parameters,
>       State(..),
>           Switching(..),
>           Constants(..),
>           Level(..),
>
>       Locked, L, Unlocked, U,
>       lock,
>
>       liftIO, lift,
>       get, modify, gets,
>       ask, asks,
>
>       run_game,
>       fork,
>
>       get_depth,
>       get_terrain,
>       get_valid_dirs,
>       get_kaart,
>       get_objects,
>       get_creatures,
>       get_player,
>       get_line_of_sight,
>       get_shortest_paths,
>       get_last_repaint_time,
>       get_paused_switch,
>       get_active_threads,
>
>       set_depth,
>       set_terrain,
>       set_valid_dirs,
>       set_kaart,
>       set_objects,
>       set_creatures,
>       set_player,
>       set_line_of_sight,
>       set_shortest_paths,
>       set_last_repaint_time,
>       set_paused_switch,
>       set_active_threads,
>
>       modify_creatures,
>       modify_player,
>       register_active_thread,
>
>       status_line
>   ) where

> import Control.Concurrent
> import Data.IORef
> import qualified Data.Ix as Ix
> import qualified Data.IntMap as IM
>
> -- from the transformers library
> import Control.Monad.Trans (liftIO)
> import Control.Monad.Reader (ask, asks, runReaderT, ReaderT, MonadReader)
> import Control.Monad.State (modify, get, gets, runStateT, StateT)

>
> import Defs
> import Util.RandomM
> import Util.Lock
> import Util.Flag
> import State.XP
> import State.Player
> import State.Creature
> import State.Health
> import State.Inventory



** Game parameters (pure) **

> type Parameters = (I, I)


** Game state (pure) **


> data State = State {
>       switching   :: Switching,
>       constants   :: Constants,
>       level       :: IORef Level
>   }

> data Switching = Switching {
>       global_lock         :: (forall a. L a -> L a), -- Lock,
>       quit_game           :: Flag
>   }

> data Constants = Constants {
>       parameters      :: Parameters,
>       dimensions      :: Pos,
>       bounds          :: (Pos, Pos),
>       all_positions   :: [Pos],
>       clock_speed     :: Int
>   }

> data Level = Level {
>       depth_              :: Integer,
>       terrain_            :: Terrain,
>       valid_dirs_         :: ValidDirs,
>       kaart_              :: Kaart,
>       objects_            :: Objects,
>       creatures_          :: Creatures,
>       player_             :: Player,
>       line_of_sight_      :: LOS,
>       shortest_paths_     :: Pathing,
>
>       last_repaint_time_  :: Integer,
>       paused_switch_      :: Switch,
>       active_threads_     :: [Flag]
>   }

> undefined_level :: Level
> undefined_level = Level {
>       depth_              = undefined,
>       terrain_            = undefined,
>       valid_dirs_         = undefined,
>       kaart_              = undefined,
>       objects_            = undefined,
>       creatures_          = undefined,
>       player_             = undefined,
>       line_of_sight_      = undefined,
>       shortest_paths_     = undefined,
>
>       last_repaint_time_  = undefined,
>       paused_switch_      = undefined,
>       active_threads_     = undefined
>   }


** Game state (impure) **

> type Locked = ReaderT State IO
> type Unlocked = StateT Level Locked

> type L = Locked
> type U = Unlocked

> instance RandomM Locked where
>   random = liftIO random
>   randomR = liftIO . randomR

> instance RandomM Unlocked where
>   random = liftIO random
>   randomR = liftIO . randomR

> lock :: Unlocked a -> Locked a
> lock u = do
>   gl <- asks (global_lock . switching)
>   gl $ do
>       l_var <- asks level
>       l0 <- liftIO $ readIORef l_var
>       (a, l1) <- runStateT u l0
>       liftIO $ writeIORef l_var l1
>       return a


> get_depth                 :: U Integer
> get_depth                 = gets depth_
>
> get_terrain               :: U Terrain
> get_terrain               = gets terrain_
>
> get_valid_dirs            :: U ValidDirs
> get_valid_dirs            = gets valid_dirs_
>
> get_kaart                 :: U Kaart
> get_kaart                 = gets kaart_
>
> get_objects               :: U Objects
> get_objects               = gets objects_
>
> get_creatures             :: U Creatures
> get_creatures             = gets creatures_
>
> get_player                :: U Player
> get_player                = gets player_
>
> get_line_of_sight         :: U LOS
> get_line_of_sight         = gets line_of_sight_
>
> get_shortest_paths        :: U Pathing
> get_shortest_paths        = gets shortest_paths_
>
> get_last_repaint_time     :: U Integer
> get_last_repaint_time     = gets last_repaint_time_
>
> get_paused_switch         :: U Switch
> get_paused_switch         = gets paused_switch_
>
> get_active_threads        :: U [Flag]
> get_active_threads        = gets active_threads_

> set_depth :: Integer -> U ()
> set_depth d       = modify $ \l -> l {depth_ = d}
>
> set_terrain :: Terrain -> U ()
> set_terrain t     = modify $ \l -> l {terrain_ = t}
>
> set_valid_dirs :: ValidDirs -> U ()
> set_valid_dirs vd = modify $ \l -> l {valid_dirs_ = vd}
>
> set_kaart :: Kaart -> U ()
> set_kaart k       = modify $ \l -> l {kaart_ = k}
>
> set_objects :: Objects -> U ()
> set_objects o     = modify $ \l -> l {objects_ = o}
>
> set_creatures :: Creatures -> U ()
> set_creatures c   = modify $ \l -> l {creatures_ = c}
>
> set_player :: Player -> U ()
> set_player p      = modify $ \l -> l {player_ = p}
>
> set_line_of_sight :: LOS -> U ()
> set_line_of_sight los = modify $ \l -> l {line_of_sight_ = los}
>
> set_shortest_paths :: Pathing -> U ()
> set_shortest_paths p = modify $ \l -> l {shortest_paths_ = p}
>
> set_last_repaint_time :: Integer -> U ()
> set_last_repaint_time t = modify $ \l -> l {last_repaint_time_ = t}
>
> set_paused_switch :: Switch -> U ()
> set_paused_switch p = modify $ \l -> l {paused_switch_ = p}
>
> set_active_threads :: [Flag] -> U ()
> set_active_threads a = modify $ \l -> l {active_threads_ = a}

> modify_creatures :: (Creatures -> Creatures) -> U ()
> modify_creatures f = modify $ \l -> l {creatures_ = f (creatures_ l)}
>
> modify_player :: (Player -> Player) -> U ()
> modify_player f = modify $ \l -> l {player_ = f (player_ l)}
>
> register_active_thread :: U Flag
> register_active_thread = do
>   f <- liftIO new_flag
>   modify $ \l -> l{active_threads_ = f : (active_threads_ l)}
>   return f



** Access to read-only state **

 class MonadReader State m => MS m where {}

 instance MS U
 instance MS L



** Run game **

> run_game :: Parameters -> L a -> IO a
> run_game params game = do
>   l <- newIORef undefined_level
>   gl <- make_lock
>   qg <- new_flag
>   let s = Switching {
>               global_lock = gl,
>               quit_game = qg
>           }
>       state = State {
>               switching = s,
>               constants = make_constants params,
>               level = l
>           }
>   runReaderT game state


> make_constants :: Parameters -> Constants
> make_constants params = Constants {
>       parameters = params,
>       dimensions = params,
>       bounds = ((1, 1), params),
>       all_positions = Ix.range ((1, 1), params),
>       clock_speed = 1000000
>   }



** Forking **

> fork :: L () -> L ()
> fork action = do
>   state <- ask
>   liftIO $ forkIO $ runReaderT action state
>   return ()

 repeat_until_halted :: L () -> L a -> L (IO ())
 repeat_until_halted = haltable_repeat fork_gs

 regular_repeat_until_halted :: Int -> L a -> L (IO ())
 regular_repeat_until_halted delay = repeat_until_halted (regular_delay delay)

 sine_repeat_until_halted :: Int -> Int -> Double -> L a -> L (IO ())
 sine_repeat_until_halted avg_delay period amplitude action = do
   rep <- make_sine_delay avg_delay period amplitude
   repeat_until_halted rep action

 sine_sync_repeat_until_halted :: Int -> Int -> Double -> L a -> L (IO ())
 sine_sync_repeat_until_halted avg_delay period amplitude =
   repeat_until_halted (sine_delay_sync avg_delay period amplitude)





** Status line **

> status_line :: U String
> status_line = gets status_line_pure

> status_line_pure :: Level -> String
> status_line_pure l =
>       let p = player_ l
>           c = creatures_ l
>           h = health (cid_map c IM.! player_cid c)
>           x = xp p
>       in
>       "    > " ++ show (depth_ l) ++
>       "    @ " ++ show (hp h) ++ "/" ++ show (max_hp h) ++
>       "    | " ++ show (xp_points x) ++ ":" ++ show (xp_level x) ++
>       "    ! " ++ show (num_potions (inventory p)) ++
>       "    ? " ++ show (num_scrolls (inventory p)) ++
>       "    ; " ++ show (hunger p)
