
> module Action.Player (
>       age_player, alive,
>       scroll, drink,
>       move_player_to
>   ) where

> import Control.Monad (when)

> import StupidClasses
> import Constants
> import Defs
> import State.Health
> import State.Creature
> import State.Player
> import State.State
> import Action.Creatures

> age_player :: U ()
> age_player = do
>   cid <- get_player_cid
>   age_creature cid
>
>   p <- get_player
>   set_player $ p {hunger = hunger p - 1}
>   when (hunger p - 1 < 0) (modify_creature cid $ \c -> c {killed = True})

> alive :: U Bool
> alive = get_player_creature >>= return . not . killed

> scroll :: U ()
> scroll = do
>   modify_player read_scroll
>   get_player_cid >>= phase_door

> drink :: U ()
> drink = do
>   modify_player drink_potion
>   get_player_cid >>= creature_drink

> move_player_to :: Pos -> U ()
> move_player_to pos = do
>   cid <- get_player_cid
>   move_creature cid pos
