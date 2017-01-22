module Main where

import Criterion.Main
import Enigma

---

main :: IO ()
main = defaultMain
  [ bgroup "Enigma.cipher"
    [ bench "single value" . nf (evalState (cipher 1)) $ Key 1
    ]
  ]
