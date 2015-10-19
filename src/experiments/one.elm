module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug

type alias Keys =
  { x : Int
  , y : Int
  }

type alias Model =
  { x : Float
  , y : Float
  , bullets : List Bullet
  }

type alias Bullet =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }

update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
  model
  |> updatePosition (dt, keys)
  |> updateShooting keys
  |> updateBullets dt

updateBullets : Float -> Model -> Model
updateBullets dt model =
  let
    bullets = List.map (\x -> { x | y <- x.y + x.dy * dt }) model.bullets
    remaining = List.filter (\x -> x.y < 300) bullets
  in
    { model | bullets <- remaining }

updatePosition : (Float, Keys) -> Model -> Model
updatePosition (dt, keys) model =
  { model |
      x <- model.x + (toFloat keys.x)
  ,   y <- model.y + (toFloat keys.y)
  }


updateShooting : Keys -> Model -> Model
updateShooting keys model =
  if keys.y > 0
     then { model | bullets <- (bullet model)::model.bullets }
     else model

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

drawBullets : Form -> List Bullet -> List Form
drawBullets form bullets =
  List.map (\b -> form |> move (b.x, b.y)) bullets

view : (Int, Int) -> Model -> Element
view (width, height) model =
  let
    cyanRectangle = rect 2 2 |> filled red
    blackishRectangle = rect 25 25 |> filled black
    position = (model.x, model.y)
    player = blackishRectangle |> move position
    bullets = drawBullets cyanRectangle model.bullets
  in
    collage width height (player::bullets)

box : Model
box =
  { x = 0
  , y = 0
  , bullets = []
  }

bullet : Model -> Bullet
bullet model =
  { x = model.x
  , y = model.y
  , dx = 0
  , dy = 3
  }

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update box input)
