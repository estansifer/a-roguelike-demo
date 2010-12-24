
> module Util.Signal (
>       one_second,
>       infinite_signal, infinite_signal_,
>       indefinite_signal, indefinite_signal_
>   ) where

> import Control.Concurrent
> import Control.Concurrent.MVar
> import Util.Util (loop, while)

> one_second :: Int
> one_second = 1000000

Returns an mvar which has the value 'x' put into it every 'time' microseconds.

> infinite_signal :: a -> Int -> IO (MVar a)
> infinite_signal x time = do
>       signal <- newEmptyMVar
>       forkIO $ loop $ do
>           threadDelay time
>           putMVar signal x
>       return signal

> infinite_signal_ :: Int -> IO (MVar ())
> infinite_signal_ = infinite_signal ()

Returns an mvar which has the value 'x' put into it every 'time' microseconds,
but stop and close the thread if the mvar 'stop' is ever nonempty.

> indefinite_signal :: MVar b -> a -> Int -> IO (MVar a)
> indefinite_signal stop x time = do
>       signal <- newEmptyMVar
>       forkIO $ (wait >>) $ while go $ do
>           putMVar signal x
>           threadDelay time
>       return signal
>   where
>       go :: IO Bool
>       go = isEmptyMVar stop
>
>       wait :: IO ()
>       wait = threadDelay time

> indefinite_signal_ :: MVar b -> Int -> IO (MVar ())
> indefinite_signal_ stop time = indefinite_signal stop () time
