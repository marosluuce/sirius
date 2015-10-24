module CollisionTest where

import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Test exposing (test, Test, suite)

import Collision exposing (..)
import Object exposing (Object)

type alias TestObject = Object {}

newTestObject : Float -> Float -> Float -> Float -> TestObject
newTestObject x y width height =
  { x = x
  , y = y
  , dx = 0
  , dy = 0
  , width = width
  , height = height
  }

tests : Test
tests =
  suite "Collision"
    [ test "objects can collide" <|
        let
          rect1 = newTestObject 0 0 1 1
        in
          assert <| collided rect1 rect1
    , test "objects can not collide" <|
        let
          rect1 = newTestObject 0 0 1 1
          rect2 = newTestObject 2 2 1 1
        in
          assertEqual False <| collided rect1 rect2
    ]
