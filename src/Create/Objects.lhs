
> module Create.Objects (
>       create_objects_m,
>       create_objects
>   ) where

> import Control.Monad.ST (runST)
> import qualified Data.Array.IArray as IA

> import Defs
> import Util.Util (arrayizeM)
> import Util.RandomM
> import TerrainComputation (random_open_location_m)

> food_density      = 0.0015
> scroll_density    = 0.001
> potion_density    = 0.0005
> stairs_density    = 0.0003

> next_p :: Double -> Double
> next_p p = if p > 0.8 then p else sqrt p

> num :: Double -> STR s Int
> num p = do
>   wbranch p (fmap (+ 1) $ num $ next_p p) (return 0)

> create_object_stack :: STR s [Object]
> create_object_stack = do
>   num_food <- num food_density
>   num_scroll <- num scroll_density
>   num_potion <- num potion_density
>   num_stairs <- num stairs_density
>   return $ replicate num_food Food ++ replicate num_scroll Scroll ++ replicate num_potion Potion ++ replicate num_stairs Stairs

> create_stairs :: Terrain -> Objects -> STR s Objects
> create_stairs terrain objects = do
>   pos <- random_open_location_m terrain
>   let o = objects IA.! pos
>   return $ objects IA.// [(pos, o ++ [Stairs])]

Given the existing terrain and a random seed, produce an array
of objects.  Objects will only appear in empty locations, and
will appear in any empty location with equal likelihood.  Objects
tend towards being stacked.  Exactly one location will have stairs.

> create_objects_m :: RandomM m => Terrain -> m Objects
> create_objects_m terrain = random >>= return . create_objects terrain

> create_objects :: Terrain -> Int -> Objects
> create_objects terrain seed = runST $ run_str seed $ do
>   let create_objects_at pos = case terrain IA.! pos of
>           Floor -> create_object_stack
>           Wall -> return []
>
>   objs1 <- arrayizeM create_objects_at (IA.bounds terrain)
>   objs2 <- create_stairs terrain objs1
>   create_stairs terrain objs2
