{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Two where

type family Not (this :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 intFunc) = T1 (fmap f intFunc)
