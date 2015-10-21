module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window

import Input exposing (Input, input)
import Draw
import Player exposing (Player, newPlayer)
import Bullet exposing (Bullet, newBullet)
import Enemy exposing (Enemy, newEnemy)

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
    , enemies <- if newEnemyRate == 0 then (newEnemy halfHeight)::enemies else enemies
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
     then { game | bullets <- (newBullet player)::bullets }
     else game

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
    playerPosition = (player.x, player.y)
    drawnPlayer = Draw.oneAt playerPosition <| playerRect player
    bulletPositions = List.map (\b -> (b.x, b.y)) bullets
    drawnBullets = Draw.manyAt bulletPositions <| List.map bulletRect bullets
    enemyPositions = List.map (\e -> (e.x, e.y)) enemies
    drawnEnemies = Draw.manyAt enemyPositions <| List.map enemyRect enemies
  in
    collage width height <|
      [ rect screenWidth screenHeight |> filled blue
      , drawnPlayer
      ] ++ drawnBullets
        ++ drawnEnemies

initialGame : Game
initialGame =
  { player = newPlayer -halfHeight
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
