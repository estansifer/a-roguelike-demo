
> module Demo (
>       module Util,
>       module BasicDefs,
>       module Queue,
>       module TerrainGen,
>       module TerrainComputation,
>
>       disp_char_grid,
>       disp_terrain,
>       disp_distances
>   ) where
>
> import Util
> import BasicDefs
> import Queue
> import TerrainGen
> import TerrainComputation

> disp_char_grid :: Grid Char -> IO ()
> disp_char_grid g = putStr $ pretty_print_char_grid g

> disp_terrain :: Terrain -> IO ()
> disp_terrain = putStr . pretty_print_level

> disp_distances :: Pathing -> IO ()
> disp_distances = disp_char_grid . fmap d where
>   d :: (Int, a) -> Char
>   d (i, _) = last $ (' ':) $ show i
