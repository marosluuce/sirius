module Input (Input, input) where

import Keyboard
import Time exposing (fps)

type alias Input =
  { x : Int
  , y : Int
  , shoot : Bool
  , delta : Float
  }

delta : Signal Float
delta = fps 30

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      (Signal.map .x Keyboard.arrows)
      (Signal.map .y Keyboard.arrows)
      Keyboard.space
      delta
