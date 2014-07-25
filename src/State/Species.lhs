
> module State.Species (
>       CreatureType(..), timed,
>       MovementType(..),
>       Species(..),
>       scarcity,
>       human,
>       all_species
>   ) where

> data CreatureType = Protagonist | Monster deriving (Eq)
>
> data MovementType =
>   Human |
>   WithHuman |
>   WithHumanSlow Double |
>   Timed Int |
>   TimedWave Int Int Double
>       deriving (Eq)
>
> timed :: MovementType -> Bool
> timed Human = False
> timed WithHuman = False
> timed (WithHumanSlow _) = False
> timed (Timed _) = True
> timed (TimedWave _ _ _) = True
>
> data Species = Species {
>       creature_type :: CreatureType,
>       movement_type :: [MovementType],
>       species_texture :: Char,
>       min_depth :: Integer,
>       max_damage :: Integer,
>       max_health :: Integer,
>       xp_reward :: Int,
>       base_scarcity :: Integer,
>       scarcity_per_level :: Integer
>   }
>
> scarcity :: Integer -> Species -> Integer
> scarcity level s =
>   if min_depth s > level then 0 else
>   (level - min_depth s) * (scarcity_per_level s) + (base_scarcity s)
>
> human :: Species
> human = Species {
>       creature_type = Protagonist,
>       movement_type = [Human],
>       species_texture = '@',
>       min_depth = 1,
>       xp_reward = 0,
>       max_damage = 4,
>       max_health = 18,
>       base_scarcity = 0,
>       scarcity_per_level = 0
>   }

How many microseconds in a millisecond...

> ms :: Int
> ms = 1000

> kobold :: Species
> kobold = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman],
>       species_texture = 'K',
>       min_depth = 0,
>       max_damage = 2,
>       max_health = 6,
>       xp_reward = 2,
>       base_scarcity = 100,
>       scarcity_per_level = -5
>   }

> hobgoblin :: Species
> hobgoblin = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman],
>       species_texture = 'H',
>       min_depth = 1,
>       max_damage = 3,
>       max_health = 7,
>       xp_reward = 3,
>       base_scarcity = 70,
>       scarcity_per_level = 5
>   }
> orc :: Species
> orc = Species {
>       creature_type = Monster,
>       movement_type = [WithHumanSlow 0.6],
>       species_texture = 'O',
>       min_depth = 4,
>       max_damage = 5,
>       max_health = 18,
>       xp_reward = 8,
>       base_scarcity = 40,
>       scarcity_per_level = 0
>   }

> emu :: Species 
> emu = Species {
>       creature_type = Monster,
>       movement_type = [Timed (400 * ms)],
>       species_texture = 'E',
>       min_depth = 2,
>       max_damage = 5,
>       max_health = 5,
>       xp_reward = 4,
>       base_scarcity = 60,
>       scarcity_per_level = 10
>   }

> quagga :: Species
> quagga = Species {
>       creature_type = Monster,
>       movement_type = replicate 10 (WithHumanSlow 0.1),
>       species_texture = 'Q',
>       min_depth = 5,
>       max_damage = 5,
>       max_health = 9,
>       xp_reward = 5,
>       base_scarcity = 40,
>       scarcity_per_level = 5
>   }

> bat :: Species
> bat = Species {
>       creature_type = Monster,
>       movement_type = [Timed (230 * ms)],
>       species_texture = 'B',
>       min_depth = 3,
>       max_damage = 2,
>       max_health = 2,
>       xp_reward = 1,
>       base_scarcity = 120,
>       scarcity_per_level = 10
>   }

> doppleganger1 :: Species
> doppleganger1 = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman],
>       species_texture = '@',
>       min_depth = 6,
>       max_damage = 6,
>       max_health = 18,
>       xp_reward = 9,
>       base_scarcity = 20,
>       scarcity_per_level = 0
>   }

> doppleganger2 :: Species
> doppleganger2 = Species {
>       creature_type = Monster,
>       movement_type = [Timed (300 * ms)],
>       species_texture = '@',
>       min_depth = 6,
>       max_damage = 6,
>       max_health = 18,
>       xp_reward = 6,
>       base_scarcity = 20,
>       scarcity_per_level = 0
>   }

> snake :: Species
> snake = Species {
>       creature_type = Monster,
>       movement_type = [TimedWave (180 * ms) (2100 * ms) (0.85)],
>       species_texture = 'S',
>       min_depth = 7,
>       max_damage = 3,
>       max_health = 8,
>       xp_reward = 4,
>       base_scarcity = 30,
>       scarcity_per_level = 0
>   }

> zombie :: Species
> zombie = Species {
>       creature_type = Monster,
>       movement_type = [WithHumanSlow 0.3],
>       species_texture = 'Z',
>       min_depth = 8,
>       max_damage = 4,
>       max_health = 45,
>       xp_reward = 6,
>       base_scarcity = 40,
>       scarcity_per_level = 5
>   }

> fly :: Species
> fly = Species {
>       creature_type = Monster,
>       movement_type = [Timed (70 * ms)],
>       species_texture = 'F',
>       min_depth = 9,
>       max_damage = 1,
>       max_health = 1,
>       xp_reward = 1,
>       base_scarcity = 30,
>       scarcity_per_level = 0
>   }

> jabberwocky :: Species
> jabberwocky = Species {
>       creature_type = Monster,
>       movement_type = [WithHuman, Timed (320 * ms)],
>       species_texture = 'J',
>       min_depth = 10,
>       max_damage = 5,
>       max_health = 27,
>       xp_reward = 13,
>       base_scarcity = 10,
>       scarcity_per_level = 5
>   }

> all_species :: [Species]
> all_species =
>   [kobold, hobgoblin, orc, emu, quagga,
>   bat, doppleganger1, doppleganger2, snake, zombie, fly, jabberwocky]
