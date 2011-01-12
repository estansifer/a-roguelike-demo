
> module State.Player (
>       Player(..),
>       new_player,
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
