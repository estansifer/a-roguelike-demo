
> module Util.Util (
>       arrayize, arrayizeM,
>       random_from,
>       random_int,
>       random_double,
>       random_bool,
>       random_weighted_bool,
>       branch, wbranch,
>       seed, more_seeds, rng,
>       while,
>       loop
>   ) where

> import Ix (Ix, range)
> import Data.Array (Array, array)
> import System.Random (RandomGen, Random, randomR, random)
> import Data.STRef (readSTRef, writeSTRef, STRef)
> import Control.Monad.ST (ST)
> import System.Random (randomIO, randoms, mkStdGen, getStdGen, StdGen)

Given a function and bounds on its input, make an array fom its values.

> arrayize :: Ix k => (k -> v) -> (k, k) -> Array k v
> arrayize f bounds = array bounds $ map (\i -> (i, f i)) $ range bounds

Monadic version.

> arrayizeM :: (Monad m, Ix k) => (k -> m v) -> (k, k) -> m (Array k v)
> arrayizeM f bounds = let keys = range bounds in do
>   values <- mapM f keys
>   return $ array bounds $ zip keys values


Random value in the specified closed interval.

> random_from :: (Random r, RandomGen g) => (r, r) -> STRef s g -> ST s r
> random_from range g_var = do
>   g <- readSTRef g_var
>   let (a, g') = randomR range g
>   writeSTRef g_var g'
>   return a
>
> random_r :: (Random r, RandomGen g) => STRef s g -> ST s r
> random_r g_var = do
>   g <- readSTRef g_var
>   let (b, g') = random g
>   writeSTRef g_var g'
>   return b
>
> random_int :: RandomGen g => STRef s g -> ST s Int
> random_int = random_r
>
> random_double :: RandomGen g => STRef s g -> ST s Double
> random_double = random_r
>
> random_bool :: RandomGen g => STRef s g -> ST s Bool
> random_bool = random_r

The parameter 'p' controls the probability of returning True.

> random_weighted_bool :: RandomGen g => Double -> STRef s g -> ST s Bool
> random_weighted_bool p g_var = do
>   g <- readSTRef g_var
>   let (x, g') = random g
>   writeSTRef g_var g'
>   return (x < p)

Randomly choose a branch with equal likelihood.

> branch :: RandomGen g => STRef s g -> ST s a -> ST s a -> ST s a
> branch g_var left right = do
>   go_left <- random_bool g_var
>   if go_left
>       then left
>       else right

Randomly choose a branch, with the first branch having the specified likelihood.

> wbranch :: RandomGen g => STRef s g -> Double -> ST s a -> ST s a -> ST s a
> wbranch g_var p left right = do
>   go_left <- random_weighted_bool p g_var
>   if go_left
>       then left
>       else right

Creates a random seed for giving to random number generators.

> seed :: IO Int
> seed = randomIO

Given a single seed, create an infinite list of seeds

> more_seeds :: Int -> [Int]
> more_seeds s = randoms (mkStdGen s)

Creates a random number generator, with a random seed.

> rng :: IO StdGen
> rng = getStdGen

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
