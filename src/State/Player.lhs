
> module State.Player (
>       Player(..),
>       new_player,
>       pick_up_obj, pick_up_objs,
>       eat,
>       has_scroll, has_potion
>   ) where
>
> import Constants
> import Defs
> import StupidClasses
> import State.Creature
> import State.XP
> import State.Inventory

> data Player = Player {
>   inventory :: Inventory,
>   xp :: XP,
>   hunger :: Integer
> }

> new_player :: Player
> new_player = Player {
>       inventory = empty_inventory,
>       xp = no_xp,
>       hunger = starting_hunger
>   }

> eat :: Player -> Player
> eat p = p {hunger = min max_hunger (hunger p + food_nourishment)}

> has_scroll :: Player -> Bool
> has_scroll p = num_scrolls (inventory p) > 0

> has_potion :: Player -> Bool
> has_potion p = num_potions (inventory p) > 0

> instance Potionable Player where
>   drink_potion p = p {
>           inventory = drink_potion (inventory p),
>           hunger = min max_hunger (hunger p + potion_nourishment)
>       }

> instance Scrollable Player where
>   read_scroll p = p {
>           inventory = read_scroll (inventory p)
>       }

> pick_up_obj :: Player -> Object -> Player
> pick_up_obj p obj = case obj of
>   Food -> eat p
>   Scroll -> p {inventory = add_scrolls (inventory p) 1}
>   Potion -> p {inventory = add_potions (inventory p) 1}
>   Stairs -> p

> pick_up_objs :: Player -> [Object] -> Player
> pick_up_objs p [] = p
> pick_up_objs p (o:os) = pick_up_objs (pick_up_obj p o) os
