
> module State.Inventory (
>       Inventory(..)
>   ) where

> import StupidClasses

> data Inventory = Inventory {
>       num_potions :: Int,
>       num_scrolls :: Int
>   }

> empty_inventory = Inventory {num_potions = 0, num_scrolls = 0}

> add_potions :: Inventory -> Int -> Inventory
> add_potions i d = i {num_potions = num_potions i + d}

> add_scrolls :: Inventory -> Int -> Inventory
> add_scrolls i d = i {num_scrolls = num_scrolls i + d}

> instance Potionable Inventory where
>   drink_potion i = add_potions i (-1)

> instance Scrollable Inventory where
>   read_scroll i = add_scrolls i (-1)
