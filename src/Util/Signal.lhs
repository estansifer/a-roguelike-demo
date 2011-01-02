
> module Util.Signal (
>       new_flag,
>       raise_flag, block_on_flag, is_raised,
>
>       Signal, act, halt, halt_all, unforked_act_on_signal,
>       new_signal, regular_signal, sine_signal, sine_signal_sync
>   ) where
>
> import System.CPUTime

for MonadIO:

> import Control.Monad.Trans
> import Control.Concurrent
> import Control.Concurrent.MVar
>
> import Util.Util (loop)

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
> is_raised = isEmptyMVar



> data Sign = Act | Halt
> type Signal = MVar Sign

> act :: Signal -> IO ()
> act signal_var = putMVar signal_var Act

> halt :: Signal -> IO ()
> halt signal_var = putMVar signal_var Halt

> halt_all :: [Signal] -> IO ()
> halt_all = mapM halt

This is intended to be forked in its own thread.

> unforked_act_on_signal :: MonadIO m => Signal -> m a -> m ()
> unforked_act_on_signal signal action = body where
>   body = do
>       sign <- liftIO $ takeMVar signal
>       case sign of
>           Act -> action >> body
>           Halt -> return ()

Returns a Signal that receives no timed signals.

> new_signal :: IO Signal
> new_signal = newEmptyMVar

Returns a Signal that receives a signal every 'delay' microseconds.

> regular_signal :: Int -> IO Signal
> regular_signal delay = do
>   signal_var <- new_signal
>   forkIO $ loop $ do
>       threadDelay delay
>       act signal_var
>   return signal_var

Returns a Signal that receives sinusoidally spaced signals.

The spacing is periodic with period 'period'.  The average delay from
one signal to the next is 'delay'.  The amplitude of the sinusoidal
signal is 'amplitude'.  The amplitude must be strictly less than 1.

The parameters are specified in microseconds, except the amplitude is unitless.

> sine_signal :: Int -> Double -> Int -> IO Signal
> sine_signal period amplitude delay = do
>   signal_var <- new_signal
>   start_time <- getCPUTime   -- in pico seconds (10^{-12})
>   forkIO $ loop $ do
>       cur_time <- getCPUTime
>       let dt = (fromInteger (cur_time - start_time)) / 1000000. :: Double
>       threadDelay $ calc_sine_delay period amplitude delay dt
>       act signal_var
>   return signal_var

Same as 'sine_signal', but all such 'sine_signal_sync's will be
synchronized with each other.

> sine_signal_sync :: Int -> Double -> Int -> IO Signal
> sine_signal_sync period amplitude delay = do
>   signal_var <- new_signal
>   forkIO $ loop $ do
>       cur_time <- getCPUTime
>       let dt = (fromInteger cur_time) / 1000000. :: Double
>       threadDelay $ calc_sine_delay period amplitude delay dt
>       act signal_var
>   return signal_var


Used to be:
    round $ (fi delay / (fi amp * k * sin (fi dt * k) + 1.0)) where
but now we have changed amplitude to be unitless.

> calc_sine_delay :: Int -> Double -> Int -> Double -> Int
> calc_sine_delay period amp delay dt =
>   let k = 2 * pi / fi period :: Double in
>   round $ (fi delay / (amp * sin (fi dt * k) + 1.0)) where
>       fi = fromIntegral
