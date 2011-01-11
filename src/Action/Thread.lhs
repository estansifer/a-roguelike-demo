
> module Action.Thread (
>       new_paused_switch,
>       is_paused,
>       unless_paused,
>       unless_paused_,
>       pause,
>       unpause,
>       repeat_until_paused,
>       block_until_paused,
>       block_until_unpaused,
>
>       fork_thread,
>       repeat_with_delay
>    ) where

> import Control.Monad (unless)

> import Util.Util (repeat_until)
> import Util.Flag

> import State.State


> new_paused_switch :: U ()
> new_paused_switch = liftIO new_switch_off >>= set_paused_switch

> is_paused :: U Bool
> is_paused = get_paused_switch >>= liftIO . is_switch_off

> unless_paused :: U () -> L ()
> unless_paused action = lock $ do
>   dont <- is_paused
>   unless dont action

> unless_paused_ :: U a -> L (Maybe a)
> unless_paused_ action = lock $ do
>   dont <- is_paused
>   if dont
>       then return Nothing
>       else fmap Just action

> pause :: U ()
> pause = get_paused_switch >>= liftIO . turn_off

> unpause :: U ()
> unpause = get_paused_switch >>= liftIO . turn_on

> repeat_until_paused :: L a -> L a
> repeat_until_paused = flip repeat_until (const $ lock is_paused)

> block_until_paused :: L ()
> block_until_paused = lock get_paused_switch >>= liftIO . block_until_off

> block_until_unpaused :: L ()
> block_until_unpaused = lock get_paused_switch >>= liftIO . block_until_on

> fork_thread :: L () -> L ()
> fork_thread action = fork $ do
>   block_until_unpaused
>   mf <- unless_paused_ register_active_thread
>   case mf of
>       Nothing -> return ()
>       Just done_flag -> action >> liftIO (raise_flag done_flag)

> repeat_with_delay :: L () -> U () -> L (IO ())
> repeat_with_delay delay action = do
>   halt_flag <- liftIO new_flag
>   let body = do
>           delay
>           halt <- liftIO $ is_raised halt_flag
>           unless halt (do
>               ma <- unless_paused_ action
>               case ma of
>                   Just _ -> body
>                   Nothing -> return ())
>   fork_thread body
>   return (raise_flag halt_flag)
