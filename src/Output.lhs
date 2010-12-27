
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
> import State.GameState

> type Canvas s = SA.STArray s Pos Char
> type S s = ST s ()

> _paint :: Canvas s -> Pos -> Char -> S s
> _paint canvas pos c = MA.writeArray canvas pos c

> _paint_vis :: LOS -> Canvas s -> Pos -> Char -> S s
> _paint_vis los canvas pos c =
>   if los IA.! pos
>       then _paint canvas pos c
>       else return ()

> repaint :: GS ()
> repaint = do
>   paint_level
>   paint_status
>   liftIO $ refresh

> paint_level :: GS ()
> paint_level = do
>   bs <- bounds
>   t <- terrain
>   poss <- all_positions
>   objs <- objects
>   los <- line_of_sight
>   loc <- location
>   k <- kaart
>
>   liftIO $ print_array_corner $ SA.runSTArray $ do
>       canvas <- MA.newArray bs unseen_character
>       paint_terrain canvas t poss
>       paint_objects canvas objs poss
>       paint_vis_monsters canvas los
>       paint_character canvas loc
>       paint_unseen canvas k poss
>       return canvas

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

> paint_status :: GS ()
> paint_status = do
>   (_, y) <- liftIO $ get_screen_size
>   line <- status_line
>   liftIO $ print_string 0 (y - 1) (line ++ "      ")



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
