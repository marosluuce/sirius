module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

type alias Input =
  { x : Int
  , y : Int
  , shoot : Bool
  , delta : Time
  }

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }

type alias Game =
  { player : Player
  , bullets : List Bullet
  }

type alias Player =
  Object { shooting : Bool
         , speed : Float
         }

type alias Bullet =
  Object { toLive : Int }

update : Input -> Game -> Game
update input game =
  game
  |> processInput input
  |> updatePosition
  |> updateShooting
  |> updateBullets

processInput : Input -> Game -> Game
processInput { x, y, shoot } ({ player } as game) =
  let
    newPlayer =
      { player |
        dx <- (toFloat x)
      , dy <- (toFloat y)
      , shooting <- shoot
      }
  in
    { game | player <- newPlayer }

updatePosition : Game -> Game
updatePosition ({ player } as game) =
  let
    { x, y, dx, dy, speed } = player
    newPlayer =
      { player |
        x <- x + dx * speed
      , y <- y + dy * speed
      }
  in
    { game | player <- newPlayer }

updateBullet : Bullet -> Bullet
updateBullet bullet =
  let
    newY = bullet.y + bullet.dy
    newToLive = bullet.toLive - 1
  in
    { bullet | y <- newY, toLive <- newToLive }

shouldKeep : Bullet -> Bool
shouldKeep { toLive } =
  toLive > 0

updateBullets : Game -> Game
updateBullets ({ bullets } as game) =
  let
    newBullets = List.map updateBullet bullets
    remainingBullets = List.filter shouldKeep newBullets
  in
    { game | bullets <- remainingBullets }

updateShooting : Game -> Game
updateShooting ({ player, bullets } as game) =
  if player.shooting
     then { game | bullets <- (bullet player)::bullets }
     else game

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

drawOne : Form -> Object a -> Form
drawOne form { x, y } =
  form |> move (x, y)

drawMany : Form -> List (Object a) -> List Form
drawMany form objects =
  List.map (drawOne form) objects

playerRect : Form
playerRect =
  rect 25 25 |> filled black

bulletRect : Form
bulletRect =
  rect 4 4 |> filled red

view : (Int, Int) -> Game -> Element
view (width, height) { player, bullets } =
  let
    drawnPlayer = drawOne playerRect player
    drawnBullets = drawMany bulletRect bullets
  in
    collage width height <| drawnPlayer::drawnBullets

bullet : Player -> Bullet
bullet model =
  { x = model.x
  , y = model.y
  , dx = 0
  , dy = 12
  , toLive = 1000
  }

initialGame : Game
initialGame =
  { player =
      { x = 0
      , y = 0
      , dx = 0
      , dy = 0
      , shooting = False
      , speed = 6
      }
  , bullets = []
  }

main : Signal Element
main =
  Signal.map2 view Window.dimensions <|
    Signal.foldp update initialGame input
