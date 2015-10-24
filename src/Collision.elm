module Collision where

import Object exposing (Object)

type alias Box =
  { top : Float
  , right : Float
  , bottom : Float
  , left : Float
  }

collided : Object a -> Object b -> Bool
collided rect1 rect2 =
  (abs (rect1.x - rect2.x)) * 2 < (rect1.width + rect2.width) &&
  (abs (rect1.y - rect2.y)) * 2 < (rect1.height + rect2.height)
