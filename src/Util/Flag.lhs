
> module Util.Flag (
>       make_flag
>   ) where
>
> import Control.Concurrent.MVar

Typical usage:

    do
        (signal, block) <- make_flag
        ...
        forkIO $ (... signal ...)
        ...
        block

> make_flag :: IO (IO (), IO ())
> make_flag = do
>   flag <- newEmptyMVar
>   return (putMVar flag (), takeMVar flag)
