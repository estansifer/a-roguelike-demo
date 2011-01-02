
in version 7 : {-# LANGUAGE ExplicitForAll #-}

> {-# LANGUAGE RankNTypes #-}
>
> module Util.Lock (
>       Lock,
>       make_lock
>   ) where
>
> import Control.Monad.Trans
>
> import Control.Concurrent.MVar

> type Lock = forall a m. MonadIO m => m a -> m a

For some reason changing 'return (...)' to 'return $ (...)' makes ghc
complain (it doesn't type check).  See information on "impredicativity".

> make_lock :: IO Lock
> make_lock = do
>   lock <- newMVar ()
>   return (\action -> do
>       liftIO $ takeMVar lock
>       a <- action
>       liftIO $ putMVar lock ()
>       return a)
