{-|
Module      : Proposition
Description : Definition of a proposition
Maintainer  : Josh Acay <cacay@cmu.edu>
Stability   : experimental
-}
module Main where

import Decider (decide)
import Proposition (Prop (Atom, T, F), (/\), (\/), (==>), (<==), (<=>), neg)
import Tester (Test, tests, check, runTests)


-- Some shortcuts
a = Atom "A"
b = Atom "B"
c = Atom "C"
d = Atom "D"
e = Atom "E"
f = Atom "F"


main :: IO ()
main = runTests tests >> return ()