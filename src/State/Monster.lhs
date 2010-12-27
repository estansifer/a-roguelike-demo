
> module State.Monster (
>       MonsterType(..),
>       MovementType(..),
>       max_health,
>       xp_reward,
>       movement_type,
>       Monster(..),
>       new_monster_at,
>       monster_step_time,
>       monster_alive
>   ) where

> import BasicDefs

> max_turns_to_next_regen = 15

> data MonsterType =
>   Kobold |
>   Emu

There is one tic every fixed period of real time.

WithPlayer:  whenever the player moves, the monster moves
Timed:  at every tic, the monster moves
WithPlayerAndTimed:  whenever the player moves, and at every tic, the monster moves
TimedWave:  the monster moves in short bursts, as a function of real time

> data MovementType =
>   WithPlayer |
>   Timed

> max_health :: MonsterType -> Int
> max_health Kobold = 6
> max_health Emu = 4

> xp_reward :: MonsterType -> Int
> xp_reward Kobold = 2
> xp_reward Emu = 4

> movement_type :: MonsterType -> MovementType
> movement_type Kobold = WithPlayer
> movement_type Emu = Timed

> monster_max_attack :: MonsterType -> Int
> monster_max_attack Kobold = 2
> monster_max_attack Emu = 5

> data Monster = Monster {
>       monster_type :: MonsterType,
>       monster_location :: Pos,
>       monster_health :: Int,
>       monster_turns_to_next_regen :: Int
>   }

> new_monster_at :: MonsterType -> Pos -> Monster
> new_monster_at mt pos = Monster {
>       monster_type = mt,
>       monster_location = pos,
>       monster_health = max_health mt,
>       monster_turns_to_next_regen = max_turns_to_next_regen
>   }

> monster_step_time :: Monster -> Monster
> monster_step_time m =
>   let hp = monster_health m
>       next_hp = monster_turns_to_next_regen m
>       max_hp = max_health (monster_type m) in
>   if hp == max_hp then m else
>   if next_hp > 0 then m {monster_turns_to_next_regen = next_hp - 1} else
>   m {monster_health = hp + 1, monster_turns_to_next_regen = max_turns_to_next_regen}

> monster_alive :: Monster -> Bool
> monster_alive m = monster_health m >= 0
