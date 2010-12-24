
> module Util.Queue (
>       STQueue,
>       empty_st_queue,
>       is_st_empty,
>       st_enqueue,
>       st_dequeue
>   ) where

> import Control.Monad.ST
> import Data.STRef

> data Queue a = Queue [a] [a]

> empty_queue :: Queue a
> empty_queue = Queue [] []

> is_empty :: Queue a -> Bool
> is_empty (Queue xs ys) = null xs && null ys

> enqueue :: Queue a -> a -> Queue a
> enqueue (Queue xs ys) x = Queue (x:xs) ys

> dequeue :: Queue a -> Maybe (a, Queue a)
> dequeue (Queue [] []) = Nothing
> dequeue (Queue xs []) = let (y:ys) = reverse xs in Just (y, Queue [] ys)
> dequeue (Queue xs (y:ys)) = Just (y, Queue xs ys)

> type STQueue s a = STRef s (Queue a)

> empty_st_queue :: ST s (STQueue s a)
> empty_st_queue = newSTRef (empty_queue)

> is_st_empty :: STQueue s a -> ST s Bool
> is_st_empty q_var = do
>   q <- readSTRef q_var
>   return (is_empty q)

> st_enqueue :: STQueue s a -> a -> ST s ()
> st_enqueue q_var x = modifySTRef q_var (`enqueue` x)

> st_dequeue :: STQueue s a -> ST s (Maybe a)
> st_dequeue q_var = do
>   q <- readSTRef q_var
>   case dequeue q of
>       Nothing -> return Nothing
>       Just (a, q') -> do
>           writeSTRef q_var q'
>           return (Just a)

