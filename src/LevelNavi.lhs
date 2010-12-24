
> module LevelNavi where
>
> import Control.Monad (forM_)
> import qualified Data.Array.IArray as IA
> import qualified Data.Array.MArray as MA
> import Data.Array.IO
> import Data.IORef
>
> import System
> import System.Random
>
> import Util.Util
> import BasicDefs
> import Util.CursesWrapper
> import TerrainComputation
> import GameState

> texture :: T -> Char
> texture Floor = '.'
> texture Wall = '#'

> main :: IO ()
> main = wrap_main $ do
>   seed <- randomIO
>   (x, y) <- get_screen_size
>   main_loop (create_game x y seed)

> main_loop :: GameState -> IO ()
> main_loop gs = do
>   display_line_of_sight gs
>   display_player gs
>   refresh
>   mgs' <- move_player gs
>   case mgs' of
>       Nothing -> return ()
>       Just gs' -> main_loop gs'

> move_player :: GameState -> IO (Maybe GameState)
> move_player gs = do
>   c <- get_char
>   if c == 'q' then return Nothing else do
>       let d = char_to_dir c
>       if can_move_in_direction gs d
>           then return $ Just $ move_player_to gs (player_location gs `add_dir` d)
>           else move_player gs

> char_to_dir :: Char -> Dir
> char_to_dir c = case c of
>   'h' -> (-1,  0)
>   'j' -> ( 0,  1)
>   'k' -> ( 0, -1)
>   'l' -> ( 1,  0)
>   'y' -> (-1, -1)
>   'u' -> ( 1, -1)
>   'b' -> (-1,  1)
>   'n' -> ( 1,  1)
>   _ -> (0, 0)

Curses is (0, 0)-based instead of (1, 1)-based.

> display_line_of_sight :: GameState -> IO ()
> display_line_of_sight gs = do
>   forM_ (map fst $ filter snd $ IA.assocs $ line_of_sight gs) $ \pos@(x, y) ->
>       print_char (x - 1) (y - 1) (texture (terrain gs IA.! pos))

> display_player :: GameState -> IO ()
> display_player gs = do
>   let (x, y) = player_location gs
>   print_char (x - 1) (y - 1) '@'
