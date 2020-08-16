module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYear * periodRatio planet)

earthYear :: Float
earthYear = 31557600

periodRatio :: Planet -> Float
periodRatio Mercury = 0.2408467
periodRatio Venus = 0.61519726
periodRatio Earth = 1
periodRatio Mars = 1.8808158
periodRatio Jupiter = 11.862615
periodRatio Saturn = 29.447498
periodRatio Uranus = 84.016846
periodRatio Neptune = 164.79132
