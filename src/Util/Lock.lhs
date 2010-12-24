
> module Util.Lock (
>       Lock,
>       make_lock
>   ) where
>
> import Control.Concurrent.MVar

> type Lock a = IO a -> IO a

> make_lock :: IO (Lock a)
> make_lock = do
>   lock <- newMVar ()
>   return $ \action -> do
>       takeMVar lock
>       a <- action
>       putMVar lock ()
>       return a
