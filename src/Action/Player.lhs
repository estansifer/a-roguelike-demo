
> module Action.Player (
>       age_player, alive,
>       scroll, drink,
>       move_player_to,
>       pick_up_object
>   ) where

> import Control.Monad (when)

> import StupidClasses
> import Constants
> import Defs
> import State.Health
> import State.Species
> import State.Creature
> import State.Inventory
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


> pick_up_food :: U ()
> pick_up_food = modify_player eat

> pick_up_scroll :: U ()
> pick_up_scroll = modify_player $ \p -> p {inventory = add_scrolls (inventory p) 1}

> pick_up_potion :: U ()
> pick_up_potion = modify_player $ \p -> p {inventory = add_potions (inventory p) 1}

> pick_up_sword :: U ()
> pick_up_sword = do
>   cid <- get_player_cid
>   cur_depth <- get_depth
>   let dam = (max_damage human) + ((cur_depth + 1) `div` 2)
>   modify_creature cid $ \c -> c {species = (species c) {max_damage = dam}}

> pick_up_object :: Object -> U ()
> pick_up_object o = case o of
>   Food -> pick_up_food
>   Scroll -> pick_up_scroll
>   Potion -> pick_up_potion
>   Sword -> pick_up_sword
>   Stairs -> return ()
