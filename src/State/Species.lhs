
> module State.Species (
>       CreatureType(..),
>       MovementType(..),
>       Species(..),
>       human,
>       kobold, emu, snake, jabberwocky,
>       all_species
>   ) where

> data CreatureType = Protagonist | Monster deriving (Eq)
>
> data MovementType =
>   Human |
>   WithHuman |
>   Timed |
>   TimedWave deriving (Eq)
>
> data Species = Species {
>       creature_type :: CreatureType,
>       movement_type :: [MovementType],
>       species_texture :: Char,
>       xp_reward :: Integer,
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
>       movement_type = [Timed],
>       species_texture = 'E',
>       xp_reward = 4,
>       max_damage = 5,
>       max_health = 4,
>       min_depth = 6,
>       scarcity = 70
>   }

> snake :: Species
> snake = Species {
>       creature_type = Monster,
>       movement_type = [TimedWave],
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
>       movement_type = [WithHuman, Timed],
>       species_texture = 'J',
>       xp_reward = 11,
>       max_damage = 4,
>       max_health = 17,
>       min_depth = 10,
>       scarcity = 10
>   }

> all_species :: [Species]
> all_species = [kobold, emu, snake, jabberwocky]
