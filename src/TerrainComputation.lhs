
> module TerrainComputation (
>       compute_valid_dirs,
>       compute_shortest_paths,
>       compute_los,
>       random_open_location,
>       random_open_location_m
>   ) where
>
> import Prelude hiding (floor)
>
> import Control.Monad (forM_)
> import Control.Monad.ST
>
> import qualified Data.Array
> import qualified Data.Array.IArray as IA
> import qualified Data.Array.MArray as MA
> import Data.Array.ST (runSTArray)
>
> import Util.Util (arrayize, while, repeat_until)
> import Util.RandomM
> import Util.Queue
> import Defs

This is used to store the line of sight calculations of 'los_path' in an array.
If the actual width or height exceeded these bounds, then we would get an
array index out of bounds error.

> max_width = 400
> max_height = 300

We keep an array storing all valid movement directions from any given position,
for faster computation later.  Assumes that all boundary positions are walls.

> compute_valid_dirs :: Terrain -> ValidDirs
> compute_valid_dirs t = arrayize (compute_valid_dirs_at t) (IA.bounds t)


We use a breadth-first search to find the shortest paths from every point
to the given point.

If a point claims a distance of 0 and direction of (0, 0), then either it is
the starting point or it is not accessible to the starting point.

> compute_shortest_paths :: ValidDirs -> Pos -> Pathing
> compute_shortest_paths dirs pos = runSTArray $ do
>       let bounds = IA.bounds dirs
>       paths <- MA.newArray bounds (0, (0, 0))
>       visited <- (MA.newArray bounds False :: ST s (MGrid s Bool))
>
>       q_var <- empty_st_queue
>       st_enqueue q_var (pos, (0, (0, 0)))
>       while (fmap not $ is_st_empty q_var) $ do
>           Just (cur, path@(dist, _)) <- st_dequeue q_var
>
>           v <- MA.readArray visited cur
>           if v then return () else do
>               MA.writeArray visited cur True
>               MA.writeArray paths cur path
>               
>               forM_ (dirs IA.! cur) $ \dir -> do
>                   v' <- MA.readArray visited cur
>                   if v' then return () else
>                       st_enqueue q_var (add_dir cur dir, (dist + 1, flip_dir dir))
>       
>       return paths

'los_path' gives a list of all positions that the line connecting
(0, 0) and (x, y) pass through.

Suppose x > 0, y >= 0, and y <= x.

Then at x-coordinate x', the line has y-coordinates varying from
    (y / x) * (x' - (1 / 2)) + (1 / 2) = (2 * y * x' - y + x) / (2 * x)
to
    (y / x) * (x' + (1 / 2)) + (1 / 2) = (2 * y * x' + y + x) / (2 * x)
inclusive.  So we round the former down to the next integer, and the latter up
to the next integer, and take the half-open range of y-coordinates that
that spans.

> los_path :: Pos -> [Pos]
> los_path (x, y) =
>   if x < 0 then map flip_x_dir (los_path (-x, y)) else
>   if y < 0 then map flip_y_dir (los_path (x, -y)) else
>   if y > x then map swap_x_y (los_path (y, x)) else
>   [(x', y') | x' <- [0..(x-1)], y' <- [low_y x'..high_y x']] where
>       low_y, high_y :: I -> I
>       low_y x' = (2 * y * x' - y + x) `div` (2 * x)
>       high_y x' = ((2 * y * x' + y + x - 1) `div` (2 * x))

> los_path_array :: Data.Array.Array Pos [Pos]
> los_path_array = arrayize los_path ((-max_width, -max_height), (max_width, max_height))

> get_los_path :: Pos -> [Pos]
> get_los_path pos = los_path_array IA.! pos

> compute_los :: Terrain -> Pos -> LOS
> compute_los terrain start@(x1, y1) = arrayize in_los (IA.bounds terrain) where
>   in_los :: Pos -> Bool
>   in_los dest@(x2, y2) =
>       and $ map (is_floor . (terrain IA.!) . add_dir start)
>           $ get_los_path (x2 - x1, y2 - y1)

> random_open_location :: Terrain -> Int -> Pos
> random_open_location terrain seed = purify seed $ random_open_location_m terrain
>
> random_open_location_m :: RandomM m => Terrain -> m Pos
> random_open_location_m terrain =
>   repeat_until
>       (random_location_m (IA.bounds terrain))
>       (return . (==Floor) . (terrain IA.!))
>   
> random_location_m :: RandomM m => (Pos, Pos) -> m Pos
> random_location_m ((xlow, ylow), (xhigh, yhigh)) = do
>   x <- randomR (xlow, xhigh)
>   y <- randomR (ylow, yhigh)
>   return (x, y)
