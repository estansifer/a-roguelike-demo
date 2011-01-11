
> module Util.Time (
>       get_time
>   ) where

> import Data.Time.Calendar
> import Data.Time.Clock

In microseconds.

> get_time :: IO Integer
> get_time = do
>   u <- getCurrentTime
>   let days = fromInteger $ toModifiedJulianDay $ utctDay u
>   let seconds = toRational (utctDayTime u) + 86400 * days
>   return (truncate (1000000 * seconds))
