
> module State.Inventory (
>       Inventory(..),
>       empty_inventory,
>       add_potions, add_scrolls
>   ) where

> import StupidClasses

> data Inventory = Inventory {
>       num_potions :: Integer,
>       num_scrolls :: Integer
>   }

> empty_inventory = Inventory {num_potions = 0, num_scrolls = 0}

> add_potions :: Inventory -> Integer -> Inventory
> add_potions i d = i {num_potions = num_potions i + d}

> add_scrolls :: Inventory -> Integer -> Inventory
> add_scrolls i d = i {num_scrolls = num_scrolls i + d}

> instance Potionable Inventory where
>   drink_potion i = add_potions i (-1)

> instance Scrollable Inventory where
>   read_scroll i = add_scrolls i (-1)
