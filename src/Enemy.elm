module Enemy (Enemy, newEnemy) where

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , width : Float
  , height : Float
  }

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
