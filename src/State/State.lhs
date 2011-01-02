
> {-# LANGUAGE TypeSynonymInstances #-}
>
> module State.State (
>       Parameters,
>       Phase(..),
>       State(..),
>       GS, liftIO, 
>       run_game,
>       get_phase, set_phase,
>       lock, fork_gs, act_on_signal
>   ) where

> import System.Random (randomIO, randomRIO)
>
> -- from the transformers library
> import qualified Control.Monad.Trans as CMT (liftIO)
> import Control.Monad.Trans.Reader
>
> import BasicDefs
> import Util.RandomM
> import Util.Lock
> import Util.Signal
> import State.Player
> import State.Creature




** Constants **

> _starting_depth = 1




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
>       shortest_paths_     :: Pathing
>   }

> data ImplicitState = ImplicitState {
>       parameters_         :: Parameters,
>       phase_var_          :: MVar Phase,
>       state_var_          :: MVar State,
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


The 'run_game' function has to create the initial game state, so in particular
it requires the game parameters.  The initial game state is 'Uninitialized'.

> run_game :: Parameters -> GS a -> IO a
> run_game params exec_game = do
>   phase_var   <- newMVar $ Uninitialized
>   state_var   <- newEmptyMVar
>   l           <- make_lock
>   let is = ImplicitState {
>           parameters_     = params,
>           phase_var_      = phase_var,
>           state_var_      = state_var,
>           global_lock_    = l
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

> act_on_signal :: Signal -> GS a -> GS ThreadId
> act_on_signal signal action = fork_gs $ unforked_act_on_signal signal action
