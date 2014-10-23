{-|
Module      : Decider
Description : Decide if a proposition is provable
Maintainer  : Josh Acay <cacay@cmu.edu>
Stability   : experimental
-}
module Decider (decide) where

import Control.Arrow (second)
import Data.List (inits, tails)
import Data.Tuple (uncurry)

import Proposition (Prop (..))


-- (invertible propositions, non-invertible propositions)
type Context = ([Prop], [Prop])


-- Add a proposition to the context
add :: Prop -> Context -> Context
add p ctx@(inv, other) = case p of
  Atom _ -> (inv, p : other)   -- Assume not-invertible
  T -> ctx                     -- Leave out since useless
  F -> ([F], [])               -- Do not need anything else
  And _ _ -> (p : inv, other)
  Or _ _ -> (p : inv, other)
  Imp (Atom _) _ -> (inv, p : other)
  Imp (Imp _ _) _ -> (inv, p : other)
  Imp _ _ -> (p : inv, other)


-- Invertible decisions
right :: Context -> Prop -> Bool
right _ T = True
right ctx (And a b) = right ctx a && right ctx b
right ctx (Imp a b) = right (add a ctx) b
right (And a b : inv, other) c = right (add a $ add b (inv, other)) c
right (F : _, _) _ = True
right (Or a b : inv, other) c =
  right (add a (inv, other)) c && right (add b (inv, other)) c
right (Imp T b : inv, other) c = right (add b (inv, other)) c
right (Imp F _ : inv, other) c = right (inv, other) c
right (Imp (And d e) b : inv, other) c =
  right (add (Imp d $ Imp e b) (inv, other)) c
right (Imp (Or d e) b : inv, other) c =
  right (add (Imp e b) $ add (Imp d b) (inv, other)) c
right ([], other) p@(Or a b) =
  left other p || right ([], other) a || right ([], other) b
right ([], other) c = left other c


-- Non-invertible decisions. The invertible assumptions should be empty.
left :: [Prop] -> Prop -> Bool
left other c = or $ map (flip elim $ c) (pulls other)


-- Reduce one non-invertible assumption and continue proof
elim :: (Prop, [Prop]) -> Prop -> Bool
elim (Atom s1, _) (Atom s2) = s1 == s2
elim (Atom s, _) _ = False
elim (Imp (Atom s) b, other) c =
  right ([], other) (Atom s) && right (add b ([], other)) c
elim (Imp (Imp d e) b, other) c =
  right (add d $ add (Imp e b) ([], other)) e && right (add b ([], other)) c



-- | Pull one element out for all elements. For example,
--
-- > pulls "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
pulls :: [a] -> [(a, [a])]
pulls xs = take (length xs) $ zipWith (second . (++)) (inits xs) breakdown
  where pull (x : xs) = (x, xs)
        breakdown = map pull (tails xs)



decide :: Prop -> Bool
decide = right ([], [])
