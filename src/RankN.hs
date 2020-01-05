{-# LANGUAGE RankNTypes #-}

module RankN where

applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5
