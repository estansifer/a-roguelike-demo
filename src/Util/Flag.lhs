
> module Util.Flag (
>       new_flag,
>       raise_flag, block_on_flag, is_raised,
>
>       new_switch_off, new_switch_on,
>       turn_on, turn_off,
>       block_until_on, block_until_off,
>       is_switch_on, is_switch_off,
>
>       haltable_repeat,
>       regular_delay,
>       make_sine_delay,
>       sine_delay_sync
>   ) where
>
> import System.CPUTime

for MonadIO:

> import Control.Monad.Trans
> import Control.Concurrent
> import Control.Concurrent.MVar
>
> import Util.Util (loop)


Flag


The flag should be raised at
most one time (if it is raised more, one of the operations will block
when not intended, usually 'raise_flag').  'is_raised' does not block, and returns
whether or not the flag has been raised yet.  'block_on_flag' returns
immediately if it has been raised, otherwise blocks until it is raised.

> type Flag = MVar ()

> new_flag :: IO Flag
> new_flag = newEmptyMVar

> raise_flag :: Flag -> IO ()
> raise_flag = flip putMVar ()

> block_on_flag :: Flag -> IO ()
> block_on_flag = readMVar

> is_raised :: Flag -> IO Bool
> is_raised = fmap not isEmptyMVar



Switch

> data Switch = Switch {
>       on_flag :: MVar (),
>       off_flag :: MVar ()
>   }

> new_switch_off :: IO Switch
> new_switch_off = do
>   on <- newEmptyMVar
>   off <- newMVar ()
>   return $ Switch {on_flag = on, off_flag = off}

> new_switch_on :: IO Switch
> new_switch_on = do
>   on <- newMVar ()
>   off <- newEmptyMVar
>   return $ Switch {on_flag = on, off_flag = off}

> turn_on :: Switch -> IO ()
> turn_on switch = do
>   takeMVar (off_flag switch)
>   putMVar (on_flag switch) ()

> turn_off :: Switch -> IO ()
> turn_off switch = do
>   takeMVar (on_flag switch)
>   putMVar (off_flag switch) ()

> block_until_on :: Switch -> IO ()
> block_until_on switch = readMVar (on_flag switch)

> block_until_off :: Switch -> IO ()
> block_until_off switch = readMVar (off_flag switch)

> is_switch_on :: Switch -> IO Bool
> is_switch_on switch = do
>   m_on <- tryTakeMVar (on_flag switch)
>   case m_on of
>       Just _ -> return True
>       Nothing -> return False

> is_switch_off :: Switch -> IO Bool
> is_switch_off switch = do
>   m_off <- tryTakeMVar (off_flag switch)
>   case m_off of
>       Just _ -> return True
>       Nothing -> return False



> haltable_repeat :: MonadIO m => (m () -> m b) -> m () -> m a -> m (IO ())
> haltable_repeat fork delay action = do
>       halt_flag <- liftIO new_flag
>       let body = do
>               delay
>               halt <- liftIO $ is_raised halt_flag
>               if halt
>                   then return ()
>                   else action >> body
>       fork body
>       return (raise_flag halt_flag)

> regular_delay :: Monad m => Int -> m ()
> regular_delay delay_amount = threadDelay delay_amount

> make_sine_delay :: MonadIO m => Int -> Int -> Double -> m (m Int)
> make_sine_delay avg_delay period amplitude = liftIO $ do
>   start_time <- getCPUTime
>   return $ liftIO $ do
>       cur_time <- getCPUTime
>       let dt = (fromInteger (cur_time - start_time)) / 1000000.
>       threadDelay $ calc_sine_delay avg_delay period amplitude dt

> sine_delay_sync :: MonadIO m => Int -> Int -> Double -> m Int
> sine_delay_sync avg_delay period amplitude = liftIO $ do
>   cur_time <- getCPUTime
>   let dt = (fromInteger cur_time) / 1000000.
>   threadDelay $ calc_sine_delay avg_delay period amplitude dt

> calc_sine_delay :: Int -> Int -> Double -> Double -> Int
> calc_sine_delay avg_delay period amp dt =
>   let k = 2 * pi / fi period :: Double in
>   round $ (fi avg_delay / (amp * sin (fi dt * k) + 1.0)) where
>       fi = fromIntegral
