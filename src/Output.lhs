
> module Output (
>       repaint
>   ) where

> import Control.Monad (forM_)
> import qualified Data.Array.IArray as IA
> import qualified Data.Array.MArray as MA
> import Control.Monad.ST
> import qualified Data.Array.ST as SA
> import Data.STRef
>
> import Util.CursesWrapper
> import BasicDefs
> import GameState

> type Canvas s = SA.STArray s Pos Char
> type S s = ST s ()

> _paint :: Canvas s -> Pos -> Char -> S s
> _paint canvas pos c = MA.writeArray canvas pos c

> _paint_vis :: LOS -> Canvas s -> Pos -> Char -> S s
> _paint_vis los canvas pos c =
>   if los IA.! pos
>       then _paint canvas pos c
>       else return ()

> repaint :: GameState -> IO ()
> repaint gs = do
>   paint_level gs
>   paint_status gs
>   refresh

> paint_level :: GameState -> IO ()
> paint_level gs = print_array_corner $ SA.runSTArray $ do
>   canvas <- MA.newArray (bounds gs) unseen_character
>   paint_terrain canvas (terrain gs) (range gs)
>   paint_objects canvas (objects gs) (range gs)
>   paint_vis_monsters canvas (line_of_sight gs)
>   paint_character canvas (player_location gs)
>   paint_unseen canvas (kaart gs) (range gs)
>   return canvas

> paint_terrain :: Canvas s -> Terrain -> [Pos] -> S s
> paint_terrain canvas terrain poss =
>   forM_ poss $ \pos ->
>       _paint canvas pos (tile_character (terrain IA.! pos))

> paint_objects :: Canvas s -> Objects -> [Pos] -> S s
> paint_objects canvas objects poss =
>   forM_ poss $ \pos ->
>       let os = objects IA.! pos in
>       if null os then return () else
>       _paint canvas pos (objects_character os)

> paint_vis_monsters :: Canvas s -> LOS -> S s
> paint_vis_monsters canvas los = return ()

> paint_character :: Canvas s -> Pos -> S s
> paint_character canvas pos = _paint canvas pos player_character

> paint_unseen :: Canvas s -> Kaart -> [Pos] -> S s
> paint_unseen canvas kaart poss =
>   forM_ poss $ \pos ->
>       if kaart IA.! pos then return () else
>       _paint canvas pos unseen_character

Paint the status line at bottom.

> paint_status :: GameState -> IO ()
> paint_status gs = do
>   (_, y) <- get_screen_size
>   print_string 0 (y - 1) (status_line gs ++ "      ")

How do we wish to represent different features as characters on screen?

> unseen_character :: Char
> unseen_character = ' '

> player_character :: Char
> player_character = '@'

> tile_character :: T -> Char
> tile_character Floor = '.'
> tile_character Wall = '#'

> object_character :: Object -> Char
> object_character Food = ';'
> object_character Scroll = '?'
> object_character Potion = '!'
> object_character Stairs = '>'

Stairs > Potion > Scroll > Food

> objects_character :: [Object] -> Char
> objects_character os = object_character $ maximum os
