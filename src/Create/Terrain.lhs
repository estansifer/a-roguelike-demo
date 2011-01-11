
> module Create.Terrain (
>       create_terrain_m,
>       create_terrain
>   ) where

> import Prelude hiding (floor)

> import Control.Monad (forM, forM_)
> import Data.Array.MArray
> import Data.Array.ST
>
> import Defs
> import Util.RandomM

> hallway_chance = 0.07
> hole_density = 0.012
> pillar_density = 0.015
> cut_larger_dimension = 0.91
> bonus_door_prob = 0.2

Consider a room, one of whose interior dimensions is of size 'length'.  We
randomly choose whether or not to subdivide that room along that dimension,
and if so, at what point to place the dividing wall (0-indexed).

                   if length >= 30 then 1    else
                   if length >= 25 then 0.9  else
                   if length >= 20 then 0.9  else
                   if length >= 15 then 0.95 else
                   if length >= 10 then 0.9  else
                   if length == 9 then 0.75 else
                   if length == 8 then 0.45 else
                   if length == 7 then 0.4  else
                   if length == 6 then 0.35 else
                   if length == 5 then 0.1  else
                   0.1 in

> choose_div_point :: I -> STR s (Maybe I)
> choose_div_point length =
>   if length < 5
>       then return Nothing
>       else wbranch hallway_chance
>           (branch (return $ Just 1) (return $ Just (length - 2)))
>           (let p = 
>                   if length >= 30 then 1    else
>                   if length >= 25 then 0.6  else  -- 0.9
>                   if length >= 20 then 0.6  else  -- 0.9
>                   if length >= 15 then 0.65 else  -- 0.95
>                   if length >= 10 then 0.6  else  -- 0.9
>                   if length == 9 then 0.75 else
>                   if length == 8 then 0.45 else
>                   if length == 7 then 0.4  else
>                   if length == 6 then 0.35 else
>                   if length == 5 then 0.1  else
>                   0.1 in
>           wbranch p
>               (fmap Just $ randomR (2, length - 3))
>               (return Nothing))

Suppose we are placing a wall of size 'length' to subdivide a room into
two smaller rooms.  We randomly choose a location (0-indexed) and a size of
a door to place in that wall.

The length of the wall must be at least 2.

> choose_door_placement :: I -> STR s (I, I)
> choose_door_placement length =
>   if length < 2 then error "Wall is too short to place door" else do
>       let min_door_size = 2
>       let max_door_size = max 2 (length `div` 3)
>       door_size <- randomR (min_door_size, max_door_size)
>       door_location <- randomR (0, length - door_size)
>       return (door_location, door_size)


Given a width 'w' and a height 'h' of a level and a random seed 's', create a random
level terrain of that width and height.

We require that 'w' and 'h' are both at least 3.

Returns an array which is indexed by positions from (1, 1) to (w, h).  Each position
in the array is either Floor or Wall.

The outer border of the level is guaranteed to be all walls.

> create_terrain_m :: RandomM m => (I, I) -> m Terrain
> create_terrain_m dims = random >>= return . create_terrain dims

> create_terrain :: (I, I) -> Int -> Terrain
> create_terrain (w, h) seed =
>   if w < 3 || h < 3 then error "Dimensions of level are too small" else
>   runSTArray $ run_str seed $ do
>       l <- lift $ newArray ((1, 1), (w, h)) Floor
>       paint_x l Wall 1 (1, h)
>       paint_x l Wall w (1, h)
>       paint_y l Wall 1 (1, w)
>       paint_y l Wall h (1, w)
>
>       place_room l (2, 2) (w - 2) (h - 2)
>
>       place_holes l w h
>       place_pillars l w h
>
>       return l

> m_maybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
> m_maybe Nothing _ = return ()
> m_maybe (Just x) k = k x

> place_room :: STGrid s T -> Pos -> I -> I -> STR s ()
> place_room l (x, y) dx dy =
>   if dx < 2 || dy < 2 then return () else
>   let p = case dx `compare` dy of
>               GT -> cut_larger_dimension
>               EQ -> 0.5
>               LT -> 1 - cut_larger_dimension
>   in wbranch p
>       (do
>           m_div_x <- choose_div_point dx
>           m_maybe m_div_x $ \div_x -> do
>               paint_x l Wall (x + div_x) (y, y + dy - 1)
>               place_x_doors l (x + div_x, y) dy
>               place_room l (x, y) div_x dy
>               place_room l (x + div_x + 1, y) (dx - div_x - 1) dy)
>       (do
>           m_div_y <- choose_div_point dy
>           m_maybe m_div_y $ \div_y -> do
>               paint_y l Wall (y + div_y) (x, x + dx - 1)
>               place_y_doors l (x, y + div_y) dx
>               place_room l (x, y) dx div_y
>               place_room l (x, y + div_y + 1) dx (dy - div_y - 1))

> place_x_doors :: STGrid s T -> Pos -> I -> STR s ()
> place_x_doors l (x, y) dy = do
>   (door_loc, door_size) <- choose_door_placement dy
>   paint_x l Floor x (y + door_loc, y + door_loc + door_size - 1)
>   wbranch bonus_door_prob
>       (place_x_doors l (x, y) dy)
>       (return ())

> place_y_doors :: STGrid s T -> Pos -> I -> STR s ()
> place_y_doors l (x, y) dx = do
>   (door_loc, door_size) <- choose_door_placement dx
>   paint_y l Floor y (x + door_loc, x + door_loc + door_size - 1)
>   wbranch bonus_door_prob
>       (place_y_doors l (x, y) dx)
>       (return ())

> paint :: STGrid s T -> T -> (I, I) -> (I, I) -> STR s ()
> paint l t (a, b) (c, d) =
>   if c < a || d < b then return () else do
>   forM_ [a..c] $ \x ->
>       forM_ [b..d] $ \y ->
>           lift $ writeArray l (x, y) t

> paint_x :: STGrid s T -> T -> I -> (I, I) -> STR s ()
> paint_x l t x (y1, y2) =
>   if (y2 < y1) then return () else do
>   forM_ [y1..y2] $ \y ->
>       lift $ writeArray l (x, y) t

> paint_y :: STGrid s T -> T -> I -> (I, I) -> STR s ()
> paint_y l t y (x1, x2) =
>   if (x2 < x1) then return () else do
>   forM_ [x1..x2] $ \x ->
>       lift $ writeArray l (x, y) t

> set :: STGrid s T -> Pos -> T -> STR s ()
> set grid pos val = lift $ writeArray grid pos val

> place_holes :: STGrid s T -> I -> I -> STR s ()
> place_holes l w h =
>   forM_ [2 .. (w - 1)] $ \x ->
>       forM_ [2..(h - 1)] $ \y -> do
>           wbranch hole_density (set l (x, y) Floor) (return ())

> place_pillars :: STGrid s T -> I -> I -> STR s ()
> place_pillars l w h =
>   forM_ [2..(w - 1)] $ \x ->
>       forM_ [2..(h - 1)] $ \y -> do
>           make_pillar <- random_weighted_bool pillar_density
>           o <- opening l x y
>           if make_pillar && o then set l (x, y) Wall else return ()

> opening :: STGrid s T -> I -> I -> STR s Bool
> opening l x y = fmap (all is_floor) $
>   forM [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1]] $ lift . readArray l
