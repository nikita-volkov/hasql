module Main where

import Prelude
import Bug
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified ConnectedTests as A


main =
  defaultMain A.tests
