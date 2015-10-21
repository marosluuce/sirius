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
import Object exposing (Object)

(screenWidth, screenHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

type alias Game =
  { player : Player
  , bullets : List Bullet
  , enemies : List Enemy
  , enemySpawnRate : Float
  , currentEnemyRate : Float
  , enemyGenerator : Generator Enemy
  }

type alias Generator a =
  { spawnRate : Float
  , currentRate : Float
  , spawnType : a
  }

enemyGenerator : Generator Enemy
enemyGenerator =
  { spawnRate = 1500
  , currentRate = 0
  , spawnType = newEnemy halfHeight
  }

newGame : Game
newGame =
  { player = newPlayer -halfHeight
  , bullets = []
  , enemies = []
  , enemySpawnRate = 1500
  , currentEnemyRate = 0
  , enemyGenerator = enemyGenerator
  }

updateGenerator : Float -> Generator a -> Generator a
updateGenerator delta ({ spawnRate, currentRate } as generator) =
  let
    newRate = currentRate + delta
    changedRate = if newRate < spawnRate then newRate else 0
  in
    { generator | currentRate <- changedRate }

updateGenerators : Float -> Game -> Game
updateGenerators delta ({ enemyGenerator } as game) =
  { game | enemyGenerator <- updateGenerator delta enemyGenerator }

update : Input -> Game -> Game
update ({ delta } as input) game =
  game
  |> processInput input
  |> updatePositions
  |> decayBullets
  |> updateShooting
  |> updateGenerators delta
  |> spawnEnemies

updateMovement : Object a -> Object a
updateMovement ({ x, y, dx, dy } as object) =
  { object |
    x <- x + dx
  , y <- y + dy
  }

updateMovements : List (Object a) -> List (Object a)
updateMovements objects =
  List.map updateMovement objects

updateEnemySpawnRate : Float -> Game -> Float
updateEnemySpawnRate delta { enemySpawnRate, currentEnemyRate } =
  let
    newEnemyRate = delta + currentEnemyRate
  in
    if newEnemyRate >= enemySpawnRate
       then 0
       else newEnemyRate

spawnEnemies : Game -> Game
spawnEnemies ({ enemies, enemyGenerator } as game) =
  let
    { currentRate, spawnType } = enemyGenerator
  in
    if currentRate == 0
       then { game | enemies <- enemyGenerator.spawnType::enemies }
       else game


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
        dx <- (toFloat x) * player.speed
      , dy <- (toFloat y) * player.speed
      , shooting <- shoot && newRate == 0
      , currentRate <- newRate
      }
  in
    { game | player <- newPlayer }

updatePositions : Game -> Game
updatePositions ({ player, bullets, enemies } as game) =
  { game |
    player <- updateMovement player
  , enemies <- updateMovements enemies
  , bullets <- updateMovements bullets
  }

decayBullet : Bullet -> Bullet
decayBullet ({ toLive } as bullet) =
  { bullet | toLive <- toLive - 1 }

decayBullets : Game -> Game
decayBullets ({ bullets } as game) =
  let
    updatedBullets = List.map decayBullet bullets
    remainingBullets = List.filter (\{ toLive } -> toLive > 0) updatedBullets
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

main : Signal Element
main =
  Signal.map2 view Window.dimensions <|
    Signal.foldp update newGame input
