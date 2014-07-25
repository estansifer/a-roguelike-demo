
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE Rank2Types #-}
>
> module Util.RandomM (
>       RandomM(..),
>       random_n,
>       random_weighted_bool,
>       branch, wbranch,
>       STR, lift,
>       run_str,
>       purify
>   ) where

> import Data.STRef
> import Control.Monad.ST
> import qualified System.Random as R
> import System.Random (Random, StdGen, mkStdGen)

From package transformers:

> import Control.Monad.Trans
> import Control.Monad.Reader


A class representing monads from which random numbers can be extracted.

> class Monad m => RandomM m where
>   random :: Random r => m r
>   randomR :: Random r => (r, r) -> m r

> random_n :: (RandomM m, Random r) => Int -> m [r]
> random_n n = sequence $ replicate n random

> random_weighted_bool :: RandomM m => Double -> m Bool
> random_weighted_bool p = fmap (< p) random
>   where
>       fmap :: Monad m => (a -> b) -> (m a -> m b)
>       fmap f k = k >>= return . f

> instance RandomM IO where
>   random = R.randomIO
>   randomR = R.randomRIO


> branch :: RandomM m => m a -> m a -> m a
> branch left right = do
>   go_left <- random
>   if go_left then left else right

> wbranch :: RandomM m => Double -> m a -> m a -> m a
> wbranch p left right = do
>   go_left <- random_weighted_bool p
>   if go_left then left else right


An instance of the RandomM class.  Inside of an ST monad, we can use an ST ref
to keep track of a random number generator.  The STR type takes care of the details
of this, using a Reader transformer to keep a global reference to a random
number generator.

We have to provide a 'run_str' function to create the initial state (i.e.,
initialize the random number generator) and execute it.

> type STR s = ReaderT (STRef s StdGen) (ST s)

> _poll :: (StdGen -> (a, StdGen)) -> STR s a
> _poll f = do
>   g_var <- ask
>   g <- lift $ readSTRef g_var
>   let (a, g') = f g
>   lift $ writeSTRef g_var g'
>   return a

> instance RandomM (STR s) where
>   random = _poll R.random
>   randomR range = _poll (R.randomR range)

We need a seed value for initializing the random number generator.

> run_str :: Int -> STR s a -> ST s a
> run_str seed str = do
>   g_var <- newSTRef $ mkStdGen seed
>   runReaderT str g_var

> purify :: Int -> (forall s. STR s a) -> a
> purify seed str = runST $ run_str seed str
