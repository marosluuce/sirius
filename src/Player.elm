module Player (Player, newPlayer) where

import Object exposing (Object)

type alias Player =
  Object { shooting : Bool
         , speed : Float
         }

newPlayer : Float -> Player
newPlayer halfHeight =
  { x = 0
  , y = halfHeight + 12.5
  , dx = 0
  , dy = 0
  , width = 25
  , height = 25
  , shooting = False
  , speed = 6
  }
