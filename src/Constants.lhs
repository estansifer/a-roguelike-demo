
> module Constants where

> starting_depth = 1

> max_hunger = 9999
> starting_hunger = max_hunger `div` 2
> starting_xp = 0
> starting_xp_level = 1

> food_nourishment = 250
> potion_nourishment = 100


The kobold is the most common creature, with scarcity 100.
One kobold should appear at the beginning of a level per 200 squares of level.

> scarcity_per = 200 * 100

Number of turns it would take for monster spawning to regenerate the
entire level, on average.

> scarcity_regen_time = 2000

Must be strictly less than 1.

> wave_amp = 0.7

In microseconds.

> wave_period = 1500000
