
> module State.Health (
>       Health(hp, max_hp)
>   ) where

> import Constants
> import StupidClasses

> data Health = Health {
>       hp :: Int,
>       max_hp :: Int,
>       until_regen :: Int
>   }

> regen_time :: Int -> Int
> regen_time max_hp = min max_regen_time (time_to_full_health `div` max_hp)

> heal_step :: Health -> Health
> heal_step h =
>   if hp h >= max_hp then h else
>   if until_regen <= 1
>       then h {hp = hp h + 1, until_regen = regen_time (max_hp h)}
>       else h {until_regen = until_regen h - 1}

> heal_potion :: Health -> Health
> heal_potion h = h {hp = min (max_hp h) hp'} where
>   hp' = hp h + min min_potion_heal ((max_hp h * potion_heal_percent) `div` 100)

> level_up_health :: Health -> Health
> level_up_health h = h {max_hp = max_hp', hp = min max_hp' (hp h + 3)} where
>   max_hp' = max_hp h + 4

> instance Timeful Health where
>   step = heal_step

> instance Potionable Health where
>   drink_potion = heal_potion

> instance Levelable Health where
>   level_up = level_up_health
