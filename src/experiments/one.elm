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
  , delta : Float
  }

type alias Object a =
  { a |
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , width : Float
  , height : Float
  }

type alias Game =
  { player : Player
  , bullets : List Bullet
  , enemies : List Enemy
  , enemySpawnRate : Float
  , currentEnemyRate : Float
  }

type alias Player =
  Object { shooting : Bool
         , speed : Float
         , fireRate : Float
         , currentRate : Float
         }

type alias Bullet =
  Object { toLive : Int }

type alias Enemy =
  Object { health : Int }

update : Input -> Game -> Game
update input game =
  game
  |> processInput input
  |> updatePosition
  |> updateShooting
  |> updateBullets
  |> spawnEnemies input
  |> updateEnemies

updateEnemy : Enemy -> Enemy
updateEnemy ({ y, dy } as enemy) =
  { enemy | y <- y + dy }

updateEnemies : Game -> Game
updateEnemies ({ enemies } as game) =
  let
    updatedEnemies = List.map updateEnemy enemies
  in
    { game | enemies <- updatedEnemies }

updateEnemySpawnRate : Float -> Game -> Float
updateEnemySpawnRate delta { enemySpawnRate, currentEnemyRate } =
  let
    newEnemyRate = delta + currentEnemyRate
  in
    if newEnemyRate >= enemySpawnRate
       then 0
       else newEnemyRate

spawnEnemies : Input -> Game -> Game
spawnEnemies { delta } ({ enemies } as game) =
  let
    newEnemyRate = updateEnemySpawnRate delta game
  in
    { game |
      currentEnemyRate <- newEnemyRate
    , enemies <- if newEnemyRate == 0 then enemy::enemies else enemies
    }

updateRate : Player -> Float -> Float
updateRate { currentRate, fireRate } delta =
  let
    newRate = currentRate + delta
  in
    if newRate >= fireRate
      then 0
      else newRate

processInput : Input -> Game -> Game
processInput { x, y, shoot, delta } ({ player } as game) =
  let
    newRate = updateRate player delta
    newPlayer =
      { player |
        dx <- (toFloat x)
      , dy <- (toFloat y)
      , shooting <- shoot && newRate == 0
      , currentRate <- newRate
      }
  in
    { game | player <- newPlayer }

updatePosition : Game -> Game
updatePosition ({ player } as game) =
  let
    { x, y, dx, dy, speed, currentRate, fireRate } = player
    newPlayer =
      { player |
        x <- x + dx * speed
      , y <- y + dy * speed
      }
  in
    { game | player <- newPlayer }

updateBullet : Bullet -> Bullet
updateBullet ({ y, dy, toLive } as bullet) =
  let
    newY = y + dy
    newToLive = toLive - 1
  in
    { bullet | y <- newY, toLive <- newToLive }

updateBullets : Game -> Game
updateBullets ({ bullets } as game) =
  let
    newBullets = List.map updateBullet bullets
    remainingBullets = List.filter (\{ toLive } -> toLive > 0) newBullets
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

drawOne : Object a -> Form -> Form
drawOne { x, y } form =
  form |> move (x, y)

drawMany : List (Object a) -> List Form -> List Form
drawMany objects forms =
  List.map2 drawOne objects forms

playerRect : Player -> Form
playerRect { width, height } =
  rect width height |> filled black

bulletRect : Bullet -> Form
bulletRect { width, height } =
  rect width height |> filled red

enemyRect : Enemy -> Form
enemyRect { width, height } =
  rect width height |> filled yellow

view : (Int, Int) -> Game -> Element
view (width, height) { player, bullets, enemies } =
  let
    drawnPlayer = drawOne player <| playerRect player
    drawnBullets = drawMany bullets <| List.map bulletRect bullets
    drawnEnemies = drawMany enemies <| List.map enemyRect enemies
  in
    collage width height <|
      [ rect screenWidth screenHeight |> filled blue
      , drawnPlayer
      ] ++ drawnBullets
        ++ drawnEnemies

enemy : Enemy
enemy =
  { x = 0
  , y = halfHeight
  , dx = 0
  , dy = -7
  , width = 16
  , height = 16
  , health = 50
  }

bullet : Player -> Bullet
bullet model =
  { x = model.x
  , y = model.y + model.height
  , dx = 0
  , dy = 12
  , width = 4
  , height = 50
  , toLive = 1000
  }

initialGame : Game
initialGame =
  { player =
      { x = 0
      , y = -halfHeight + 12.5
      , dx = 0
      , dy = 0
      , width = 25
      , height = 25
      , shooting = False
      , speed = 6
      , fireRate = 300
      , currentRate = 0
      }
  , bullets = []
  , enemies = []
  , enemySpawnRate = 1500
  , currentEnemyRate = 0
  }

(screenWidth, screenHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

main : Signal Element
main =
  Signal.map2 view Window.dimensions <|
    Signal.foldp update initialGame input
