
> module Create.Objects (
>       create_objects_m,
>       create_objects
>   ) where

> import Control.Monad.ST (runST)
> import qualified Data.Array.IArray as IA
> import Control.Monad (forM)

> import Defs
> import Util.Util (arrayizeM)
> import Util.RandomM
> import TerrainComputation (random_open_location_m)

> all_objects = [Food, Scroll, Potion, Sword, Stairs]
> density :: Object -> Double
> density Food      = 0.0015
> density Scroll    = 0.0010
> density Potion    = 0.0005
> density Sword     = 0
> density Stairs    = 0.0003

> next_p :: Double -> Double
> next_p p = if p > 0.8 then p else sqrt p

> num :: Double -> STR s Int
> num p = do
>   wbranch p (fmap (+ 1) $ num $ next_p p) (return 0)

> create_object_stack :: STR s [Object]
> create_object_stack = do
>   objs <- forM all_objects $ \obj -> do
>       n <- num (density obj)
>       return $ replicate n obj
>   return $ concat objs

> create_one :: Object -> Terrain -> Objects -> STR s Objects
> create_one obj terrain objects = do
>   pos <- random_open_location_m terrain
>   let o = objects IA.! pos
>   return $ objects IA.// [(pos, o ++ [obj])]

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
>   objs2 <- create_one Stairs terrain objs1
>   objs3 <- create_one Stairs terrain objs2
>   create_one Sword terrain objs3
