
> module State.Inventory (
>       Inventory(..)
>   ) where

> data Inventory = Inventory {
>       num_potions :: Int,
>       num_scrolls :: Int
>   }

> empty_inventory = Inventory {num_potions = 0, num_scrolls = 0}

> add_potions :: Inventory -> Int -> Inventory
> add_potions i d = i {num_potions = num_potions i + d}

> add_scrolls :: Inventory -> Int -> Inventory
> add_scrolls i d = i {num_scrolls = num_scrolls i + d}

