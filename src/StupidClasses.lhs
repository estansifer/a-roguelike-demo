
> module StupidClasses (
>       Timeful(..),
>       Potionable(..),
>       Scrollable(..),
>       Levelable(..)
>   ) where

> class Timeful t where
>   step :: t -> t

> class Potionable p where
>   drink_potion :: p -> p

> class Scrollable s where
>   read_scroll :: s -> s

> class Levelable l where
>   level_up :: l -> l
