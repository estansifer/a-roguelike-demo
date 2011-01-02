
> module State.XP (
>       XP(..),
>       gain_xp
>   ) where
>
> data XP = XP {
>       xp_points :: Int,
>       xp_level :: Int
>   }

Returns the new XP and how many levels were gained.  'amount' can be negative.

> gain_xp :: XP -> Int -> (XP, Int)
> gain_xp xp amount = (xp {xp_points = points', xp_level = level'}, dl) where
>   points' = xp_points xp + amount
>   level' = what_level xp'
>   dl = level' - xp_level xp

Returns what level corresponds to the number of experience points

> what_level :: Int -> Int
> what_level = logb where
>   logb :: Int -> Int
>   logb k = if k < 6 then 0 else 1 + (logb ((k * 3) `div` 4))
