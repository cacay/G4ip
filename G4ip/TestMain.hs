{-|
Module      : Main
Description : Run the default test suite
Maintainer  : Josh Acay <cacay@cmu.edu>
Stability   : experimental
-}
module Main where

import Control.Monad (void, when)
import System.Exit (exitFailure)

import G4ip.Decider (decide)
import G4ip.Tester (tests, runTests)


main :: IO ()
main = do
  errorCount <- runTests tests
  when (errorCount > 0)
    exitFailure
