module Main where

import Enigma
import Test.Tasty
import Test.Tasty.QuickCheck as QC

---

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck"
  [ QC.testProperty "cipher isomorphism" $
    \k v -> evalState (cipher (evalState (cipher v) k)) k == v
  ]
