{-|
Module      : Proposition
Description : Representation of propositions
Maintainer  : Josh Acay <cacay@cmu.edu>
Stability   : experimental
-}
module Proposition (
    Prop (..)
  , (/\)
  , (\/)
  , (==>)
  , (<==)
  , (<=>)
  , neg) where


data Prop = Atom String
          | T
          | F
          | And Prop Prop
          | Or Prop Prop
          | Imp Prop Prop
            deriving (Eq, Ord)


infixr 3 /\
infixr 2 \/
infixr 1 ==>
infixr 1 <==
infixr 0 <=>


(/\) :: Prop -> Prop -> Prop
a /\ b = And a b


(\/) :: Prop -> Prop -> Prop
a \/ b = Or a b


(==>) :: Prop -> Prop -> Prop
a ==> b = Imp a b


(<==) :: Prop -> Prop -> Prop
a <== b = Imp b a


(<=>) :: Prop -> Prop -> Prop
a <=> b = And (Imp a b) (Imp b a)


neg :: Prop -> Prop
neg a = a ==> F


instance Show Prop where
  show = showImp
    where
      showImp (Imp a b) = showAndOr a ++ " ==> " ++ showAndOr b
      showImp p = showAndOr p

      showAndOr (And a b) = showAtom a ++ " /\\ " ++ showAtom b
      showAndOr (Or a b) = showAtom a ++ " \\/ " ++ showAtom b
      showAndOr p = showAtom p

      showAtom (Atom s) = s
      showAtom T = "T"
      showAtom F = "F"
      showAtom p = "(" ++ showImp p ++ ")"
