module Bullet (Bullet, newBullet) where

import Player exposing (Player)

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , width : Float
  , height : Float
  }

type alias Bullet =
  Object { toLive : Int }

newBullet : Player -> Bullet
newBullet model =
  { x = model.x
  , y = model.y + model.height
  , dx = 0
  , dy = 12
  , width = 4
  , height = 50
  , toLive = 1000
  }
