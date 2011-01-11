
> module State.Species (
>       CreatureType(..), timed,
>       MovementType(..),
>       Species(..),
>       human,
>       kobold, emu, bat, snake, jabberwocky,
>       all_species
>   ) where

> data CreatureType = Protagonist | Monster deriving (Eq)
>
> data MovementType =
>   Human |
>   WithHuman |
>   Timed Int |
>   TimedWave Int Int Double
>       deriving (Eq)
>
> timed :: MovementType -> Bool
> timed Human = False
> timed WithHuman = False
> timed (Timed _) = True
> timed (TimedWave _ _ _) = True
>
> data Species = Species {
>       creature_type :: CreatureType,
>       movement_type :: [MovementType],
>       species_texture :: Char,
>       xp_reward :: Int,
>       max_damage :: Integer,
>       max_health :: Integer,
>       min_depth :: Integer,
>       scarcity :: Integer
>   }
>
> human :: Species
> human = Species {
>       creature_type = Protagonist,
>       movement_type = [Human],
>       species_texture = '@',
>       xp_reward = 0,
>       max_damage = 6,
>       max_health = 15,
>       min_depth = 1,
>       scarcity = 0
>   }

How many microseconds in a millisecond...

> ms :: Int
> ms = 1000

> kobold :: Species
> kobold = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman],
>       species_texture = 'K',
>       xp_reward = 2,
>       max_damage = 2,
>       max_health = 6,
>       min_depth = 1,
>       scarcity = 100
>   }

> emu :: Species 
> emu = Species {
>       creature_type = Monster,
>       movement_type = [Timed (400 * ms)],
>       species_texture = 'E',
>       xp_reward = 4,
>       max_damage = 5,
>       max_health = 4,
>       min_depth = 6,
>       scarcity = 60
>   }

> bat :: Species
> bat = Species {
>       creature_type = Monster,
>       movement_type = [Timed (230 * ms)],
>       species_texture = 'B',
>       xp_reward = 2,
>       max_damage = 2,
>       max_health = 2,
>       min_depth = 7,
>       scarcity = 120
>   }

> snake :: Species
> snake = Species {
>       creature_type = Monster,
>       movement_type = [TimedWave (110 * ms) (1600 * ms) (0.7)],
>       species_texture = 'S',
>       xp_reward = 7,
>       max_damage = 2,
>       max_health = 8,
>       min_depth = 8,
>       scarcity = 30
>   }

> jabberwocky :: Species
> jabberwocky = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman, Timed (320 * ms)],
>       species_texture = 'J',
>       xp_reward = 11,
>       max_damage = 4,
>       max_health = 17,
>       min_depth = 10,
>       scarcity = 10
>   }

> all_species :: [Species]
> all_species = [kobold, emu, bat, snake, jabberwocky]
