
> module Util.Util (
>       arrayize, arrayizeM,
>       while,
>       loop,
>       repeat_until, repeat_while,
>       db
>   ) where

> import Control.Monad.Trans

> import Ix (Ix, range)
> import Data.Array (Array, array)

Given a function and bounds on its input, make an array fom its values.

> arrayize :: Ix k => (k -> v) -> (k, k) -> Array k v
> arrayize f bounds = array bounds $ map (\i -> (i, f i)) $ range bounds

Monadic version.

> arrayizeM :: (Monad m, Ix k) => (k -> m v) -> (k, k) -> m (Array k v)
> arrayizeM f bounds = let keys = range bounds in do
>   values <- mapM f keys
>   return $ array bounds $ zip keys values


Repeat the given action 'body' so long as 'condition' yields True.

> while :: Monad m => m Bool -> m a -> m ()
> while condition body =
>   condition >>= \b ->
>   if b
>       then body >> while condition body
>       else return ()

Repeat the given action 'body' forever.

> loop :: Monad m => m a -> m b
> loop body = body >> loop body

> repeat_until :: Monad m => m a -> (a -> m Bool) -> m a
> repeat_until body condition = do
>   a <- body
>   b <- condition a
>   if b then return a else repeat_until body condition

> repeat_while :: Monad m => m a -> (a -> m Bool) -> m a
> repeat_while body condition = do
>   a <- body
>   b <- condition a
>   if b then repeat_while body condition else return a

> db :: MonadIO m => String -> m ()
> db s = liftIO $ putStrLn s
