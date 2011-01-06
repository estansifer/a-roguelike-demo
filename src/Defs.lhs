
> {-# LANGUAGE BangPatterns #-}
>
> module Defs (
>       I, Pos, Dir,
>       Grid, MGrid,
>       T(..), Object(..),
>       Terrain, Pathing, Objects, ValidDirs, LOS, Kaart,
>
>       is_floor, is_wall,
>
>       grid_width, grid_height,
>
>       flip_dir, flip_x_dir, flip_y_dir, swap_x_y, add_dir, sub_pos,
>       norm,
>       compute_valid_dirs_at,
>
>       pretty_print_level, pretty_print_char_grid
>   ) where

> import Debug.Trace

> import qualified Data.Array
> import qualified Data.Array.ST


'I' is the index type by which positions in the level are represented.

> type I = Int
> type Pos = (I, I)
> type Dir = (I, I)

> type Grid a = Data.Array.Array Pos a
> type MGrid s a = Data.Array.ST.STArray s Pos a

> data T = Floor | Wall deriving Eq

> data Object = Food | Scroll | Potion | Stairs deriving (Eq, Ord)

> type Terrain = Grid T
> type Pathing = Grid (Int, Dir)
> type Objects = Grid [Object]
> type ValidDirs = Grid [Dir]
> type LOS = Grid Bool
> type Kaart = Grid Bool

Utility functions for walls and floors.

> is_floor, is_wall :: T -> Bool
> is_floor !Floor = True
> is_floor !Wall  = False
> is_wall  !Floor = False
> is_wall  !Wall  = True

Grid height and width.

> grid_width :: Grid a -> I
> grid_width g =
>   let ((x1, _), (x2, _)) = Data.Array.bounds g in
>   x2 - x1 + 1

> grid_height :: Grid a -> I
> grid_height g =
>   let ((_, y1), (_, y2)) = Data.Array.bounds g in
>   y2 - y1 + 1


** Movement **

> hor_deltas, diag_deltas, deltas :: [Dir]
> hor_deltas = [(-1, 0), (0, -1), (0, 1), (1, 0)]
> diag_deltas = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
> deltas = hor_deltas ++ diag_deltas

> flip_dir, flip_x_dir, flip_y_dir, swap_x_y :: Dir -> Dir
> flip_dir (a, b) = (-a, -b)
> flip_x_dir (a, b) = (-a, b)
> flip_y_dir (a, b) = (a, -b)
> swap_x_y (a, b) = (b, a)

> add_dir :: Pos -> Dir -> Pos
> add_dir (!a, !b) (!c, !d) = (a + c, b + d)

> sub_pos :: Pos -> Pos -> Dir
> sub_pos (a, b) (c, d) = (a - c, b - d)

> norm :: Dir -> I
> norm (a, b) = a * a + b * b

Given the terrain 't' and a position 'p', returns a list of all directions
from to 'p', including the zero direction.  Assumes that if 'p' is on the
boundary, then 'p' must be a wall.

> compute_valid_dirs_at :: Terrain -> Pos -> [Dir]
> compute_valid_dirs_at t p =
>   if not $ pass p
>       then []
>       else (0, 0) :
>           (filter (pass . add_dir p) hor_deltas) ++
>           (filter (all_pass p) diag_deltas)
>           
>   where
>       pass :: Pos -> Bool
>       pass = is_floor . (t Data.Array.!)
>
>       all_pass :: Pos -> Dir -> Bool
>       all_pass p d =
>           pass (p `add_dir` d) &&
>           pass (p `add_dir` x_part d) &&
>           pass (p `add_dir` y_part d)
>
>       x_part, y_part :: Pos -> Pos
>       x_part (x, y) = (x, 0)
>       y_part (x, y) = (0, y)


** Grid Display **

We cheat a little and display the transpose, actually.

> texture :: T -> Char
> texture Floor = '.'
> texture Wall = '#'
>
> pretty_print_level :: Terrain -> String
> pretty_print_level l = pretty_print_char_grid $ fmap texture l
>
> pretty_print_char_grid :: Grid Char -> String
> pretty_print_char_grid g = let h = fromIntegral $ grid_height g in
>       unlines $ clump h $ Data.Array.elems g
>   where
>       clump :: Int -> [a] -> [[a]]
>       clump _ [] = []
>       clump n xs = (take n xs : clump n (drop n xs))
