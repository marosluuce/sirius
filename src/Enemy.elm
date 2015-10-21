module Enemy (Enemy, newEnemy) where

import Object exposing (Object)

type alias Enemy =
  Object { health : Int }

newEnemy : Float -> Float -> Enemy
newEnemy x y =
  { x = x
  , y = y
  , dx = 0
  , dy = -7
  , width = 16
  , height = 16
  , health = 50
  }
