{-# LANGUAGE RankNTypes #-}

module Cont where

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap aTb (Cont aTr) = Cont (\bTr -> aTr $ bTr . aTb)

instance Applicative Cont where
  pure aTb = Cont aTb
  (Cont aTb) <*> x = fmap aTb x

-- remember that :i (.)
-- (b -> r) -> (a -> b) -> a -> r
-- and
-- aTb :: a -> b
-- bTr :: b -> r
-- bTr . aTb :: a -> r
