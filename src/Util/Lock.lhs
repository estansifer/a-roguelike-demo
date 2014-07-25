
in version 7 :

> {-# LANGUAGE ExplicitForAll #-} -- GHC7

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ImpredicativeTypes #-}
>
> module Util.Lock (
>       Lock(MkLock),  -- GHC7
>       make_lock
>   ) where
>
> import Control.Monad.Trans
>
> import Control.Concurrent.MVar

> data Lock = MkLock (forall a m. MonadIO m => m a -> m a)  -- GHC7

For some reason changing 'return (...)' to 'return $ (...)' makes ghc
complain (it doesn't type check).  See information on "impredicativity".

> make_lock :: IO Lock
> make_lock = do
>   lock <- newMVar ()
>   return $ MkLock (\action -> do  -- GHC7
>       liftIO $ takeMVar lock
>       a <- action
>       liftIO $ putMVar lock ()
>       return a)  -- GHC7
