module Main where

import BasePrelude hiding (assert, isRight, isLeft)
import Control.Monad.IO.Class
import Test.QuickCheck.Instances
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SmallCheck
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck


main =
  defaultMain tree

tree =
  testGroup ""
  [
  ]
