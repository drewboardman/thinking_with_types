{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Two where

-- Show a has kind CONSTRAINT
-- Show has kind CONSTRAINT -> TYPE
--
-- Prelude Control.Monad.Trans> :k Functor
-- Functor :: (* -> *) -> Constraint
-- Prelude Control.Monad.Trans> :k Monad
-- Monad :: (* -> *) -> Constraint
-- Prelude Control.Monad.Trans> :k MonadTrans
-- MonadTrans :: ((* -> *) -> * -> *) -> Constraint

type family Not (this :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 intFunc) = T1 (fmap f intFunc)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 aii) = T5 (\bi -> aii $ bi . f)
