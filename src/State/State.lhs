
> {-# LANGUAGE TypeSynonymInstances #-}
>
> module State.State (
>       Parameters,
>       Phase(..),
>       State(..),
>       GS, liftIO, 
>       run_game,
>       get_phase, set_phase,
>       get_state, set_state,
>       get_last_repaint, set_last_repaint,
>       lock, fork_gs,
>       repeat_until_halted,
>       regular_repeat_until_halted,
>       sine_repeat_until_halted,
>       sine_sync_repeat_until_halted
>   ) where

> import System.Random (randomIO, randomRIO)
> import System.CPUTime
>
> -- from the transformers library
> import qualified Control.Monad.Trans as CMT (liftIO)
> import Control.Monad.Trans.Reader
>
> import Defs
> import Util.RandomM
> import Util.Lock
> import Util.Flag
> import State.Player
> import State.Creature




** Game parameters (pure) **

> type Parameters = (I, I)




** Global game state (pure) **

We don't export 'OngoingState' so as not to expose the internal state of the
game while it is inprogress.  We do export 'Uninitialized', 'Ongoing', 'Over',
and 'EndCondition' so that the end game state is exposed.


> data Phase = Uninitialized | Ongoing

> data State = State {
>       dims_               :: Pos,
>       bounds_             :: (Pos, Pos),
>       all_positions_      :: [Pos],
>
>       dungeon_depth_      :: Int,
>

We would need to include a random number generator to gain pure behavior,
but because everything is taking place in the GS monad, we can just use
the IO standard random number generator.

>       -- _gen :: StdGen,
>
>       terrain_            :: Terrain,
>       valid_dirs_         :: ValidDirs,
>
>       kaart_              :: Kaart,
>
>       objects_            :: Objects,
>
>       creatures_          :: Creatures,
>

The remaining fields are intimately linked to the player state, and must
be updated on every movement of the player.  Some of the arrays could
be replaced with IO arrays for greater efficiency.

>       player_             :: Player,
>       player_location_    :: Pos,
>       line_of_sight_      :: LOS,
>       shortest_paths_     :: Pathing,
>
>       paused_switch_      :: Switch
>   }

> data ImplicitState = ImplicitState {
>       parameters_         :: Parameters,
>       phase_var_          :: MVar Phase,
>       state_var_          :: MVar State,
>       last_repaint_var_   :: IORef Integer,
>       global_lock_        :: Lock
>   }




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

> type GS = ReaderT ImplicitState IO

> liftIO :: IO a -> GS a
> liftIO = CMT.liftIO

> instance RandomM GS where
>   random      = liftIO randomIO
>   randomR r   = liftIO $ randomRIO r




> parameters :: GS Parameters
> parameters = asks parameters_
>
> phase_var :: GS (MVar Phase)
> phase_var = asks phase_var_
>
> state_var :: GS (MVar State)
> state_var = asks state_var_
>
> last_repaint_var :: GS (IORef Integer)
> last_repaint_var = asks last_repaint_var_


The 'run_game' function has to create the initial game state, so in particular
it requires the game parameters.  The initial game state is 'Uninitialized'.

> run_game :: Parameters -> GS a -> IO a
> run_game params exec_game = do
>   phase_var   <- newMVar $ Uninitialized
>   state_var   <- newEmptyMVar
>   repaint_var <- newIORef 0
>   l           <- make_lock
>   let is = ImplicitState {
>           parameters_         = params,
>           phase_var_          = phase_var,
>           state_var_          = state_var,
>           last_repaint_var_   = repaint_var,
>           global_lock_        = l
>       }
>   runReaderT exec_game is


> get_state :: GS State
> get_state = state_var >>= liftIO . readMVar

This must be single-threaded.

> set_state :: State -> GS ()
> set_state s = do
>   s_var <- state_var
>   liftIO $ tryTakeMVar s_var
>   liftIO $ putMVar s_var s

This must be single-threaded.

> modify_state :: (State -> State) -> GS ()
> modify_state f = do
>   s_var <- state_var
>   liftIO $ modifyMVar_ s_var (return . f)

> get_phase :: GS Phase
> get_phase = phase_var >>= liftIO . readMVar

This must be single-threaded.

> set_phase :: Phase -> GS ()
> set_phase p = do
>   p_var <- phase_var
>   liftIO $ tryTakeMVar p_var
>   liftIO $ putMVar p_var p



> get_last_repaint_ :: GS Integer
> get_last_repaint_ = last_repaint_var >>= liftIO . readIORef

> set_last_repaint :: GS ()
> set_last_repaint = do
>   last_repaint <- last_repaint_var
>   liftIO (getCPUTime >>= writeIORef last_repaint)

> lock :: GS a -> GS a
> lock action = do
>   l <- asks global_lock_
>   l action


Given an action in the GS monad, turn it into an action in the IO monad, and
then fork it.

> fork_gs :: GS a -> GS ThreadId
> fork_gs action = do
>   implicit <- ask
>   liftIO $ forkIO $ runReaderT action implicit

> repeat_until_halted :: GS () -> GS a -> GS (IO ())
> repeat_until_halted = haltable_repeat fork_gs

> regular_repeat_until_halted :: Int -> GS a -> GS (IO ())
> regular_repeat_until_halted delay = repeat_until_halted (regular_delay delay)

> sine_repeat_until_halted :: Int -> Int -> Double -> GS a -> GS (IO ())
> sine_repeat_until_halted avg_delay period amplitude action = do
>   rep <- make_sine_delay avg_delay period amplitude
>   repeat_until_halted rep action

> sine_sync_repeat_until_halted :: Int -> Int -> Double -> GS a -> GS (IO ())
> sine_sync_repeat_until_halted avg_delay period amplitude =
>   repeat_until_halted (sine_delay_sync avg_delay period amplitude)
