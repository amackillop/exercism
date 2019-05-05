module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock {minutes :: Int}

instance Show Clock where
    show clock = printf "%02d:" (hour) ++ printf "%02d" (minute)
        where 
            hour   = getHour clock
            minute = getMinute clock

instance Eq Clock where
    (==) this that = getHour this == getHour that && 
                     getMinute this == getMinute that

getMinute :: Clock -> Int
getMinute clock = minutes clock `mod` 60

getHour :: Clock -> Int
getHour clock = (minutes clock `div` 60) `mod` 24

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock (60 * hour + minute)

toString :: Clock -> String
toString clock = show clock

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute clock = fromHourMin hour $ minute + minutes clock
