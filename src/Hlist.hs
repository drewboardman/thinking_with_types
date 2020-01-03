{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Hlist where

import           Data.Kind                      ( Constraint
                                                , Type
                                                )

-- this is saying that ts has kind [Type]
-- you don't really need to put this, but it's good practice to explicitly declare the 
-- kind of something that isn't just "Type"

data HList (ts :: [Type]) where
  HNil ::HList '[]
  -- this HAS to lead with a colon, since it is a symbolic data constructor
  (:#) ::t -> HList ts -> HList (t ': ts)
infixr 5 :#

hlength :: HList ts -> Int
hlength HNil      = 0
hlength (_ :# ts) = 1 + hlength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _ ) = t
