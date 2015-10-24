module Main where

import String

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Runner.Element exposing (runDisplay)

import CollisionTest

tests : Test
tests =
  suite
    "All die rolling specs"
    [ CollisionTest.tests
    ]

main = runDisplay tests
