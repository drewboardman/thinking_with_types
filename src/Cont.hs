{-# LANGUAGE RankNTypes #-}

module Cont where

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap aTb (Cont aTr) = Cont (\bTr -> aTr $ bTr . aTb)

instance Applicative Cont where
  pure a = Cont $ \k -> k a
  (Cont caTb) <*> (Cont ca) = Cont $ \k ->
    caTb $ \aTb ->
      ca $ \a ->
        k (aTb a)

instance Monad Cont where
  (Cont ca) >>= f = Cont $ \k ->
    ca $ \a ->
      unCont (f a) k

-- remember that :i (.)
-- (b -> r) -> (a -> b) -> a -> r
-- and
-- aTb :: a -> b
-- bTr :: b -> r
-- bTr . aTb :: a -> r
