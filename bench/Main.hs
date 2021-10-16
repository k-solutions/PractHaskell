module Main where

import           Criterion.Main
import           DataTypes

main = defaultMain [
  bgroup "map to set" [ bench whnf test 1
                      ]
  ]

