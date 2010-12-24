
> module CreateMonsters (
>   ) where

> import Monster

> monster_density :: MonsterType -> Int -> Double
> monster_density Kobold _ = 0.002
> monster_density Emu l = if l < 4 then 0 else 0.002

> 
