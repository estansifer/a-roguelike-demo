
> module Action.Player (
>       modify_player,
>       age_player, alive,
>       scroll, drink
>   ) where

> import Constants
> import Defs

> modify_player :: (Player -> Player) -> GS ()
> modify_player f = modify_state $ (\s -> s {player_ = f (player_ s)})

> age_player :: GS ()
> age_player = do
>   get_player_cid >>= age_creature
>   modify_player $ \p -> p {hunger = hunger p - 1}

> alive :: GS Bool
> alive = do
>   c <- get_player_cid >>= get_creature
>   player <- get_player
>   return (hp (health c) >= 0 && hunger player >= 0)

> scroll :: GS ()
> scroll = do
>   modify_player read_scroll
>   get_player_cid >>= phase_door

> drink :: GS ()
> drink = do
>   modify_player drink_potion
>   get_player_cid >>= creature_drink

> move_player_to :: Pos -> GS ()
> move_player_to pos = do
>   set_player_location pos
>   cid <- get_player_cid
>   update_creature_location cid pos
