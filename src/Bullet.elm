module Bullet (Bullet, newBullet) where

import Object exposing (Object)
import Player exposing (Player)

type alias Bullet =
  Object { toLive : Int
         , damage : Float
         }

newBullet : Float -> Float -> Bullet
newBullet x y =
  { x = x
  , y = y
  , dx = 0
  , dy = 12
  , width = 4
  , height = 50
  , toLive = 1000
  , damage = 500
  }
