
> module Output (
>       repaint,
>       repaint_force,
>       hard_refresh
>   ) where

> import Control.Monad (forM_, when)
> import qualified Data.Array.IArray as IA
> import qualified Data.Array.MArray as MA
> import Control.Monad.ST
> import qualified Data.Array.ST as SA
> import Data.STRef
>
> import Util.CursesWrapper
> import Util.Time
> import Constants
> import Defs
> import State.Species
> import State.State
> import State.Creature
> import Action.Creatures
> import Action.Player

> type Canvas s = SA.STArray s Pos Char
> type S s = ST s ()

> _paint :: Canvas s -> Pos -> Char -> S s
> _paint canvas pos c = MA.writeArray canvas pos c

> _paint_vis :: LOS -> Canvas s -> Pos -> Char -> S s
> _paint_vis los canvas pos c =
>   if los IA.! pos
>       then _paint canvas pos c
>       else return ()

> repaint :: U ()
> repaint = do
>   prev <- get_last_repaint_time
>   now <- liftIO get_time
>   when (now - prev > repaint_interval) repaint_force

> repaint_force :: U ()
> repaint_force = do
>   paint_level
>   paint_status
>   liftIO $ refresh
>   now <- liftIO get_time
>   set_last_repaint_time now

> hard_refresh :: U ()
> hard_refresh = do
>   liftIO $ blank_screen '_'
>   liftIO $ refresh
>   repaint_force

TODO only paint monsters we can see

> paint_level :: U ()
> paint_level = do
>   bounds      <- asks (bounds . constants)
>   terrain     <- get_terrain
>   poss        <- asks (all_positions . constants)
>   objects     <- get_objects
>   los         <- get_line_of_sight
>   creatures   <- get_living_creatures
>   kaart       <- get_kaart
>   loc         <- get_player_location
>   is_alive    <- alive
>
>   liftIO $ print_array_corner $ SA.runSTArray $ do
>       canvas <- MA.newArray bounds unseen_character
>       paint_terrain canvas terrain poss
>       paint_objects canvas objects poss
>       mapM_ (paint_creature_if_in_los canvas los) creatures
>       if not is_alive
>           then paint_dead_character canvas loc
>           else return ()
>       paint_unseen canvas kaart poss
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

Unused.  The player is a particular species now.

> paint_character :: Canvas s -> Pos -> S s
> paint_character canvas pos = _paint canvas pos player_character

> paint_dead_character :: Canvas s -> Pos -> S s
> paint_dead_character canvas pos = _paint canvas pos dead_player_character

> paint_creature_if_in_los :: Canvas s -> LOS -> Creature -> S s
> paint_creature_if_in_los canvas los creature =
>   when (los IA.! (location creature))
>       (_paint canvas (location creature) (species_texture $ species creature))

> paint_unseen :: Canvas s -> Kaart -> [Pos] -> S s
> paint_unseen canvas kaart poss =
>   forM_ poss $ \pos ->
>       if kaart IA.! pos then return () else
>       _paint canvas pos unseen_character



Paint the status line at bottom.

> paint_status :: U ()
> paint_status = do
>   (_, y) <- liftIO $ get_screen_size
>   line <- status_line
>   liftIO $ print_string 0 (y - 1) (line ++ replicate 200 ' ')



How do we wish to represent different features as characters on screen?

(The textures associated with the different species are stored in
their Species record, which are defined in various files.)

> unseen_character :: Char
> unseen_character = ' '

> player_character :: Char
> player_character = '@'

> dead_player_character :: Char
> dead_player_character = '*'

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
