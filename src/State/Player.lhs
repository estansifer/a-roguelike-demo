
> module State.Player (
>       Player(..),
>       new_player,
>       step_time,
>       pick_up_obj, pick_up_objs,
>       alive,
>       has_scroll, has_potion,
>       drink_potion, read_scroll,
>   ) where
>
> import Defs

> max_hunger = 9999
> food_nourishment = 250
> potion_nourishment = 100

> data Player = Player {
>   player_cid :: CID,
>   inventory :: Inventory,
>   xp :: Int,
>   xp_level :: Int,
>   hunger :: Int
> }

> new_player :: Player
> new_player = Player {
>       player_cid = undefined,
>       inventory = empty_inventory,
>       xp = 0,
>       xp_level = 1,
>       hunger = max_hunger `div` 2
>   }

> step_time :: Player -> Player
> step_time p =
>   let hp = health p
>       next_hp = turns_to_next_regen p
>       max_hp = max_health p in
>   (if hp == max_hp then p else
>   if next_hp > 0 then p {turns_to_next_regen = next_hp - 1} else
>   p {health = hp + 1, turns_to_next_regen = regen_time max_hp})
>
>   {hunger = hunger p - 1}

> alive :: Player -> Bool
> alive p = health p >= 0 && hunger p >= 0

> eat :: Player -> Player
> eat p = p {hunger = min max_hunger (hunger p + food_nourishment)}

> has_scroll :: Player -> Bool
> has_scroll p = num_scrolls (inventory p) > 0

> has_potion :: Player -> Bool
> has_potion p = num_potions (inventory p) > 0

> drink_potion :: Player -> Player
> drink_potion p = p {
>       inventory = add_potions (inventory p) (-1),
>       health = min (max_health p) (health p +
>           min min_potion_heal ((max_health p * potion_heal_percent) `div` 100)),
>       hunger = min max_hunger (hunger p + potion_nourishment)
>   }

> read_scroll :: Player -> Player
> read_scroll p = p {
>       inventory = add_scrolls (inventory p) (-1)
>   }

> pick_up_obj :: Player -> Object -> Player
> pick_up_obj p obj = case obj of
>   Food -> eat p
>   Scroll -> p {inventory = add_scrolls (inventory p) 1}
>   Potion -> p {inventory = add_potions (inventory p) 1}
>   Stairs -> p

> pick_up_objs :: Player -> [Object] -> Player
> pick_up_objs p [] = p
> pick_up_objs p (o:os) = pick_up_objs (pick_up_obj p o) os

