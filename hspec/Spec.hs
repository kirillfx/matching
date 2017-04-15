-- |

module Main where

import           MatchingSpec
import           Test.Hspec


main :: IO ()
main = hspec $
  sequence_ [ matchingSpec ]
