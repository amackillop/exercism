module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

data Orbits = 
    Orbits { mercury :: Float
           , venus :: Float           
           , earth :: Float
           , mars :: Float
           , jupiter :: Float
           , saturn :: Float
           , uranus :: Float
           , neptune :: Float
           }
           
earthOrbit :: Float
earthOrbit = 31557600

orbits :: Orbits
orbits = Orbits 
    (0.2408467 * earthOrbit)
    (0.61519726 * earthOrbit)
    (earthOrbit)
    (1.8808158 * earthOrbit)
    (11.862615 * earthOrbit)
    (29.447498 * earthOrbit)
    (84.016846 * earthOrbit)
    (164.79132 * earthOrbit)

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
    Mercury -> age mercury
    Venus   -> age venus
    Earth   -> age earth
    Mars    -> age mars
    Jupiter -> age jupiter
    Saturn  -> age saturn
    Uranus  -> age uranus
    Neptune -> age neptune
    where
        age p = seconds / p orbits

