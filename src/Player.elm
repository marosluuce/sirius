module Player (Player, newPlayer) where

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , width : Float
  , height : Float
  }

type alias Player =
  Object { shooting : Bool
         , speed : Float
         , fireRate : Float
         , currentRate : Float
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
  , fireRate = 300
  , currentRate = 0
  }
