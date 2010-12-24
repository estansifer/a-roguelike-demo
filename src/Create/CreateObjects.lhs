
> module CreateObjects (
>       create_objects
>   ) where

> import Control.Monad.ST
> import qualified Data.Array.IArray as IA
> import Data.STRef
> import System.Random

> import Util.Util (arrayizeM, random_int, random_double, wbranch)
> import BasicDefs
> import TerrainComputation (random_empty_location)

> food_density      = 0.0015
> scroll_density    = 0.001
> potion_density    = 0.0005
> stairs_density    = 0.0003

> next_p :: Double -> Double
> next_p p = if p > 0.8 then p else sqrt p

> num :: RandomGen r => STRef s r -> Double -> ST s Int
> num g_var p = do
>   wbranch g_var p (num g_var (next_p p) >>= \n -> return (n + 1)) (return 0)

> create_object_stack :: RandomGen r => STRef s r -> ST s [Object]
> create_object_stack g_var = do
>   num_food <- num g_var food_density
>   num_scroll <- num g_var scroll_density
>   num_potion <- num g_var potion_density
>   num_stairs <- num g_var stairs_density
>   return $ replicate num_food Food ++ replicate num_scroll Scroll ++ replicate num_potion Potion ++ replicate num_stairs Stairs

> create_stairs :: RandomGen r => Terrain -> STRef s r -> Objects -> ST s Objects
> create_stairs terrain g_var objects = do
>   s <- random_int g_var
>   let pos = random_empty_location terrain s
>   let o = objects IA.! pos
>   return $ objects IA.// [(pos, o ++ [Stairs])]

Given the existing terrain and a random seed, produce an array
of objects.  Objects will only appear in empty locations, and
will appear in any empty location with equal likelihood.  Objects
tend towards being stacked.  Exactly one location will have stairs.

> create_objects :: Terrain -> Int -> Objects
> create_objects terrain seed = runST $ do
>   g_var <- newSTRef (mkStdGen seed)
>
>   let create_objects_at pos = case terrain IA.! pos of
>           Floor -> create_object_stack g_var
>           Wall -> return []
>
>   objs1 <- arrayizeM create_objects_at (IA.bounds terrain)
>   objs2 <- create_stairs terrain g_var objs1
>   create_stairs terrain g_var objs2
