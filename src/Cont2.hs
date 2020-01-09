{-# LANGUAGE ViewPatterns #-}

module Cont2 where

import Control.Monad.Cont

doContStuff :: Cont r (a -> b -> c) -> Cont r a -> Cont r b -> Cont r c
doContStuff (runCont -> caTbTc) (runCont -> ca) (runCont -> cb) = cont $ \k ->
  caTbTc $ \aTbTc ->
    cb $ \b ->
      ca $ \a ->
        k $ aTbTc a b
