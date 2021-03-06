
> module Util.Flag (
>       Flag,
>       new_flag,
>       raise_flag, block_on_flag, is_raised,
>
>       Switch(),
>       new_switch_off, new_switch_on,
>       turn_on, turn_off,
>       block_until_on, block_until_off,
>       is_switch_on, is_switch_off,
>
>       regular_delay,
>       make_sine_delay,
>       sine_delay_sync,
>       calc_sine_delay
>   ) where
>

for MonadIO:

> import Control.Monad.Trans
> import Control.Concurrent
> import Control.Concurrent.MVar
>
> import Util.Util (loop)
> import Util.Time
> import Util.RandomM


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
> is_raised = fmap not . isEmptyMVar



Switch

> data Switch = Switch {
>       on_flag :: MVar (),
>       off_flag :: MVar (),
>       is_on :: MVar Bool
>   }

> new_switch_off :: IO Switch
> new_switch_off = do
>   on <- newEmptyMVar
>   off <- newMVar ()
>   io <- newMVar False
>   return $ Switch {on_flag = on, off_flag = off, is_on = io}

> new_switch_on :: IO Switch
> new_switch_on = do
>   on <- newMVar ()
>   off <- newEmptyMVar
>   io <- newMVar True
>   return $ Switch {on_flag = on, off_flag = off, is_on = io}

> turn_on :: Switch -> IO ()
> turn_on switch = do
>   False <- takeMVar (is_on switch)
>   takeMVar (off_flag switch)
>   putMVar (on_flag switch) ()
>   putMVar (is_on switch) True

> turn_off :: Switch -> IO ()
> turn_off switch = do
>   True <- takeMVar (is_on switch)
>   takeMVar (on_flag switch)
>   putMVar (off_flag switch) ()
>   putMVar (is_on switch) False

> block_until_on :: Switch -> IO ()
> block_until_on switch = readMVar (on_flag switch)

> block_until_off :: Switch -> IO ()
> block_until_off switch = readMVar (off_flag switch)

> is_switch_on :: Switch -> IO Bool
> is_switch_on switch = readMVar (is_on switch)

> is_switch_off :: Switch -> IO Bool
> is_switch_off switch = fmap not $ readMVar (is_on switch)



> regular_delay :: MonadIO m => Int -> m ()
> regular_delay delay_amount = liftIO $ threadDelay delay_amount

> make_sine_delay :: (MonadIO m, RandomM m) => Int -> Int -> Double -> m (IO ())
> make_sine_delay avg_delay period amplitude = liftIO $ do
>   start_time <- random
>   return $ seq start_time $ do
>       cur_time <- get_time
>       let dt = fromInteger (cur_time - start_time)
>       liftIO $ threadDelay $ calc_sine_delay avg_delay period amplitude dt

> sine_delay_sync :: MonadIO m => Int -> Int -> Double -> m ()
> sine_delay_sync avg_delay period amplitude = liftIO $ do
>   cur_time <- get_time
>   let dt = (fromInteger cur_time) / 1000000.0
>   liftIO $ threadDelay $ calc_sine_delay avg_delay period amplitude dt

> calc_sine_delay :: Int -> Int -> Double -> Double -> Int
> calc_sine_delay avg_delay period amp dt =
>   let k = 2 * pi / fi period :: Double in
>   round $ (fi avg_delay / (amp * sin (dt * k) + 1.0)) where
>       fi = fromIntegral
