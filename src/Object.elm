module Object (Object) where

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , width : Float
  , height : Float
  }
