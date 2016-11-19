-- |
-- Module    : Statistics.GModeling.DSL
-- Copyright : (c) 2016 Naren Sundaravaradan
-- License   : BSD3
--
-- Maintainer  : nano.naren@gmx.com

module Statistics.GModeling.DSL
  (
  ) where

-- | For specifying the index of a name
data Indexed a = Only a | a :@ [a]

-- | Captures a node generated by an indexed node
data Edge a = Indexed a :-> a

-- | A network is just a collection of edges
type Network a = [Edge a]