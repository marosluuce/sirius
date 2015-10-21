module Draw (oneAt, manyAt) where

import Graphics.Collage exposing (Form, move)

oneAt : (Float, Float) -> Form -> Form
oneAt position form =
  form |> move position

manyAt : List (Float, Float) -> List Form -> List Form
manyAt positions forms =
  List.map2 oneAt positions forms

