module Main where

import Criterion.Main
import Enigma

---

main :: IO ()
main = defaultMain
  [ bgroup "Enigma.cipher"
    [ bench "Word64" . nf (evalState (cipher 1)) $ Key 1
    , bench "[Word64]" . nf (evalState (traverse cipher [1..10])) $ Key 1
    ]
  ]
