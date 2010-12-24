
> module CreateTerrain (
>       create_terrain
>   ) where

> import Prelude hiding (floor)

> import Control.Monad.ST
> import Control.Monad (forM, forM_)
> import Data.Array.MArray
> import Data.Array.ST
> import Data.STRef
> import System.Random
>
> import BasicDefs
> import Util.Util (random_from, random_weighted_bool, branch, wbranch)

> hallway_chance = 0.07
> hole_density = 0.012
> pillar_density = 0.015
> cut_larger_dimension = 0.91
> bonus_door_prob = 0.2

Consider a room, one of whose interior dimensions is of size 'length'.  We
randomly choose whether or not to subdivide that room along that dimension,
and if so, at what point to place the dividing wall (0-indexed).

> choose_div_point :: RandomGen g => I -> STRef s g -> ST s (Maybe I)
> choose_div_point length g_var =
>   if length < 5
>       then return Nothing
>       else wbranch g_var hallway_chance
>           (branch g_var (return $ Just 1) (return $ Just (length - 2)))
>           (let p = 
>                   if length >= 30 then 1    else
>                   if length >= 25 then 0.9  else
>                   if length >= 20 then 0.9  else
>                   if length >= 15 then 0.95 else
>                   if length >= 10 then 0.9  else
>                   if length == 9 then 0.75 else
>                   if length == 8 then 0.45 else
>                   if length == 7 then 0.4  else
>                   if length == 6 then 0.35 else
>                   if length == 5 then 0.1  else
>                   0.1 in
>           wbranch g_var p
>               (fmap Just $ random_from (2, length - 3) g_var)
>               (return Nothing))

Suppose we are placing a wall of size 'length' to subdivide a room into
two smaller rooms.  We randomly choose a location (0-indexed) and a size of
a door to place in that wall.

The length of the wall must be at least 2.

> choose_door_placement :: RandomGen g => I -> STRef s g -> ST s (I, I)
> choose_door_placement length g_var =
>   if length < 2 then error "Wall is too short to place door" else do
>       let min_door_size = 2
>       let max_door_size = max 2 (length `div` 3)
>       door_size <- random_from (min_door_size, max_door_size) g_var
>       door_location <- random_from (0, length - door_size) g_var
>       return (door_location, door_size)


Given a width 'w' and a height 'h' of a level and a random seed 's', create a random
level terrain of that width and height.

We require that 'w' and 'h' are both at least 3.

Returns an array which is indexed by positions from (1, 1) to (w, h).  Each position
in the array is either Floor or Wall.

The outer border of the level is guaranteed to be all walls.

> create_terrain :: I -> I -> Int -> Terrain
> create_terrain w h s =
>   if w < 3 || h < 3 then error "Dimensions of level are too small" else
>   runSTArray $ do
>       g_var <- newSTRef (mkStdGen s)
>       l <- newArray ((1, 1), (w, h)) Floor
>       paint_x l Wall 1 (1, h)
>       paint_x l Wall w (1, h)
>       paint_y l Wall 1 (1, w)
>       paint_y l Wall h (1, w)
>
>       place_room l g_var (2, 2) (w - 2) (h - 2)
>
>       place_holes l w h g_var
>       place_pillars l w h g_var
>
>       return l

> m_maybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
> m_maybe Nothing _ = return ()
> m_maybe (Just x) k = k x

> place_room :: RandomGen g => MGrid s T -> STRef s g -> Pos -> I -> I -> ST s ()
> place_room l g_var (x, y) width height =
>   if width < 2 || height < 2 then return () else
>   let p = case width `compare` height of
>               GT -> cut_larger_dimension
>               EQ -> 0.5
>               LT -> 1 - cut_larger_dimension
>   in wbranch g_var p
>       (do
>           m_div_x <- choose_div_point width g_var
>           m_maybe m_div_x $ \div_x -> do
>               paint_x l Wall (x + div_x) (y, y + height - 1)
>               place_x_doors l g_var (x + div_x, y) height
>               place_room l g_var (x, y) div_x height
>               place_room l g_var (x + div_x + 1, y) (width - div_x - 1) height)
>       (do
>           m_div_y <- choose_div_point height g_var
>           m_maybe m_div_y $ \div_y -> do
>               paint_y l Wall (y + div_y) (x, x + width - 1)
>               place_y_doors l g_var (x, y + div_y) width
>               place_room l g_var (x, y) width div_y
>               place_room l g_var (x, y + div_y + 1) width (height - div_y - 1))

> place_x_doors :: RandomGen g => MGrid s T -> STRef s g -> Pos -> I -> ST s ()
> place_x_doors l g_var (x, y) height = do
>   (door_loc, door_size) <- choose_door_placement height g_var
>   paint_x l Floor x (y + door_loc, y + door_loc + door_size - 1)
>   wbranch g_var bonus_door_prob
>       (place_x_doors l g_var (x, y) height)
>       (return ())

> place_y_doors :: RandomGen g => MGrid s T -> STRef s g -> Pos -> I -> ST s ()
> place_y_doors l g_var (x, y) width = do
>   (door_loc, door_size) <- choose_door_placement width g_var
>   paint_y l Floor y (x + door_loc, x + door_loc + door_size - 1)
>   wbranch g_var bonus_door_prob
>       (place_y_doors l g_var (x, y) width)
>       (return ())

> paint :: MGrid s T -> T -> (I, I) -> (I, I) -> ST s ()
> paint l t (a, b) (c, d) =
>   if c < a || d < b then return () else do
>   forM_ [a..c] $ \x ->
>       forM_ [b..d] $ \y ->
>           writeArray l (x, y) t

> paint_x :: MGrid s T -> T -> I -> (I, I) -> ST s ()
> paint_x l t x (y1, y2) =
>   if (y2 < y1) then return () else do
>   forM_ [y1..y2] $ \y ->
>       writeArray l (x, y) t

> paint_y :: MGrid s T -> T -> I -> (I, I) -> ST s ()
> paint_y l t y (x1, x2) =
>   if (x2 < x1) then return () else do
>   forM_ [x1..x2] $ \x ->
>       writeArray l (x, y) t

> set :: MGrid s T -> Pos -> T -> ST s ()
> set = writeArray

> place_holes :: RandomGen g => MGrid s T -> I -> I -> STRef s g -> ST s ()
> place_holes l w h g_var =
>   forM_ [2 .. (w - 1)] $ \x ->
>       forM_ [2..(h - 1)] $ \y -> do
>           wbranch g_var hole_density (set l (x, y) Floor) (return ())

> place_pillars :: RandomGen g => MGrid s T -> I -> I -> STRef s g -> ST s ()
> place_pillars l w h g_var =
>   forM_ [2..(w - 1)] $ \x ->
>       forM_ [2..(h - 1)] $ \y -> do
>           make_pillar <- random_weighted_bool pillar_density g_var
>           o <- opening l x y
>           if make_pillar && o then set l (x, y) Wall else return ()

> opening :: MGrid s T -> I -> I -> ST s Bool
> opening l x y = fmap (all is_floor) $
>   forM [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1]] $ readArray l

demo :: I -> I -> Int -> IO ()
demo w h s = putStr $ pretty_print_level $ create_terrain w h s
