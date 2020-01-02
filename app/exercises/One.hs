module One where

-- Determine the cardinality of Either Bool (Bool, Maybe Bool) -> Bool.
-- This is both a sum and product type, so it's:
-- |Bool| + (|Bool| * (1 + |Bool|) ^ |Bool|
-- (2 + (2 * 3)) ^ 2 = 64
--
-- Use Curry–Howard to prove that (a^b)^c = a^(b×c). That is, provide a function of
-- type (b -> c -> a) -> (b, c) -> a, and one of ((b,c)->a)->b->c->a. Makesure they
-- satisfy the equalities to . from = id and from . to = id. Do these functions
-- remind you of anything from Prelude?
--
-- (a^b)^c => c -> b -> a => (c -> b) -> a => a^(bxc)
-- a^(bxc) => (b,c) -> a => 
