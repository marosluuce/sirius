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
import Collision exposing (collided)

(screenWidth, screenHeight) = (800, 600)
(halfWidth, halfHeight) = (400, 300)

type alias Game =
  { player : Player
  , bullets : List Bullet
  , enemies : List Enemy
  , enemyGenerator : Generator Enemy
  , bulletGenerator : Generator Bullet
  }

type alias Generator a =
  { x : Float
  , y : Float
  , spawnRate : Float
  , currentRate : Float
  , spawnType : (Float -> Float -> a)
  }

bulletGenerator : Generator Bullet
bulletGenerator =
  { x = 0
  , y = 0
  , spawnRate = 800
  , currentRate = 0
  , spawnType = newBullet
  }

enemyGenerator : Generator Enemy
enemyGenerator =
  { x = 0
  , y = halfHeight
  , spawnRate = 1500
  , currentRate = 0
  , spawnType = newEnemy
  }

newGame : Game
newGame =
  { player = newPlayer -halfHeight
  , bullets = []
  , enemies = []
  , enemyGenerator = enemyGenerator
  , bulletGenerator = bulletGenerator
  }

generate : Float -> Generator a -> (Generator a, Maybe a)
generate delta ({ x, y, spawnRate, currentRate, spawnType } as generator) =
  let
    newRate = currentRate - delta
    newType = spawnType x y
  in
    if newRate <= 0
      then ({ generator | currentRate <- spawnRate }, Just newType)
      else ({ generator | currentRate <- newRate }, Nothing)

collideBullet : List Bullet -> Enemy -> (Enemy, List Bullet)
collideBullet bullets ({ health } as enemy) =
  let
    collidedBullets = List.filter (collided enemy) bullets
    damage = List.map (.damage) collidedBullets
    totalDamage = List.sum damage
  in
    ({ enemy | health <- health - totalDamage }, collidedBullets)

handleCollision : Game -> Game
handleCollision ({ bullets, enemies } as game) =
  let
    collisions = List.map (collideBullet bullets) enemies
    updatedEnemies = List.map fst collisions
    collidedBullets = List.concatMap snd collisions
    updatedBullets = List.filter (\x -> not <| List.member x collidedBullets) bullets
  in
    { game |
      enemies <- updatedEnemies
    , bullets <- updatedBullets
    }

update : Input -> Game -> Game
update ({ delta } as input) game =
  game
  |> processInput input
  |> handleCollision
  |> cleanup
  |> updateShooting
  |> spawnEnemies delta
  |> updatePositions

decayBullet : Bullet -> Bullet
decayBullet ({ toLive } as bullet) =
  { bullet | toLive <- toLive - 1 }

keepEnemy : Enemy -> Bool
keepEnemy { y, height, health } =
  let
    top = y + height / 2
  in
    health > 0 && top > -halfHeight

cleanup : Game -> Game
cleanup ({ bullets, enemies } as game) =
  let
    updatedBullets = List.map decayBullet bullets
    remainingBullets = List.filter (\{ toLive } -> toLive > 0) updatedBullets
    remainingEnemies = List.filter keepEnemy enemies
  in
    { game |
      bullets <- remainingBullets
    , enemies <- remainingEnemies
    }


updateMovement : Object a -> Object a
updateMovement ({ x, y, dx, dy } as object) =
  { object |
    x <- x + dx
  , y <- y + dy
  }

updateMovements : List (Object a) -> List (Object a)
updateMovements objects =
  List.map updateMovement objects

spawnEnemies : Float -> Game -> Game
spawnEnemies delta ({ enemies, enemyGenerator } as game) =
  let
    (newGenerator, maybeEnemy) = generate delta enemyGenerator
    newEnemies = case maybeEnemy of
                   Just enemy -> enemy::enemies
                   Nothing -> enemies
  in
    { game | enemyGenerator <- newGenerator, enemies <- newEnemies }

processInput : Input -> Game -> Game
processInput input ({ player, bulletGenerator } as game) =
  let
    { x, y, shoot, delta } = input
    (newBulletGenerator, maybeBullet) = generate delta bulletGenerator
    shooting = case maybeBullet of
                 Just bullet -> shoot
                 Nothing -> False
    newPlayer =
      { player |
        dx <- (toFloat x) * player.speed
      , dy <- (toFloat y) * player.speed
      , shooting <- shooting
      }
  in
    { game |
      player <- newPlayer
    , bulletGenerator <- newBulletGenerator
    }

updatePositions : Game -> Game
updatePositions ({ player, bullets, enemies } as game) =
  { game |
    player <- updateMovement player
  , enemies <- updateMovements enemies
  , bullets <- updateMovements bullets
  }

updateShooting : Game -> Game
updateShooting ({ player, bullets } as game) =
  if player.shooting
     then { game | bullets <- (newBullet player.x player.y)::bullets }
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
      [ rect screenWidth screenHeight |> filled blue]
      ++ drawnBullets
      ++ drawnEnemies
      ++ [drawnPlayer]

main : Signal Element
main =
  Signal.map2 view Window.dimensions <|
    Signal.foldp update newGame input
