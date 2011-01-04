
> module Action.Pause (
>       new_paused_flag,
>       is_paused,
>       unless_paused,
>       pause,
>       unpause,
>       repeat_until_paused,
>       block_until_paused,
>       block_until_unpaused
>    ) where

> import Util.Util (repeat_until)

> import State.State
> import State.MState


> new_paused_switch :: GS ()
> new_paused_switch = new_switch_off >>= set_paused_switch

> is_paused :: GS Bool
> is_paused = get_paused_switch >>= is_switch_off

> unless_paused :: GS () -> GS ()
> unless_paused action = lock $ do
>   dont <- is_paused
>   if dont
>       then return ()
>       else action

> pause :: GS ()
> pause = get_paused_switch >>= turn_off

> unpause :: GS ()
> unpause = get_paused_switch >>= turn_on

> repeat_until_paused :: GS a -> GS a
> repeat_until_paused = repeat_until (const is_paused)

> block_until_paused :: GS ()
> block_until_paused = get_paused_switch >>= block_until_off

> block_until_unpaused :: GS ()
> block_until_unpaused = get_paused_switch >>= block_until_on