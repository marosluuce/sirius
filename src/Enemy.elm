module Enemy (Enemy, newEnemy) where

import Object exposing (Object)

type alias Enemy =
  Object { health : Int }

newEnemy : Float -> Enemy
newEnemy halfHeight =
  { x = 0
  , y = halfHeight
  , dx = 0
  , dy = -7
  , width = 16
  , height = 16
  , health = 50
  }
