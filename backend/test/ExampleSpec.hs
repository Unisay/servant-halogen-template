module ExampleSpec (run) where

import Hedgehog
import Preamble

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

run :: IO Bool
run = checkParallel $ Group "ExampleSpec"
  [ exampleProperty
  ]

exampleProperty :: (PropertyName, Property)
exampleProperty = ("Example property",) $ property $ do
  i <- forAll $ Gen.int $ Range.singleton 42
  i === 42
