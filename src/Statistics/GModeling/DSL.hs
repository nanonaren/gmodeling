-- |
-- Module    : Statistics.GModeling.DSL
-- Copyright : (c) 2016 Naren Sundaravaradan
-- License   : BSD3
--
-- Maintainer  : nano.naren@gmx.com

module Statistics.GModeling.DSL
  (
    Indexed (..)
  , Edge (..)
  , Network
  , nodes
  , children
  , parents
  , observed
  , latent
  , priors
  , indexOf
  ) where

import Control.Monad (msum)
import Data.Maybe (mapMaybe)
import Data.List (nub,(\\))

-- | For specifying the index of a
data Indexed a b = Only a | a :@ b

-- | Captures a node generated by an indexed node
data Edge a = Indexed a a :-> a

-- | A network is just a collection of edges
type Network a = [Edge a]

-- | Enumerating the nodes in the network
nodes :: Eq a => Network a -> [a]
nodes = nub . concatMap f
  where f (Only a :-> b) = [a,b]
        f ((p :@ _) :-> a) = [p, a]

-- | Enumerating the children of a node
children :: Eq a => Network a -> a -> [a]
children xs a = concatMap f xs
  where f (Only p :-> c) | p == a = [c]
        f ((p :@ i) :-> c) | p == a || i == a = [c]
        f _ = []

-- | Enumerating the parents of a node
parents :: Eq a => Network a -> a -> [a]
parents xs a = concatMap f xs
  where f (Only p :-> c) | c == a = [p]
        f ((p :@ _) :-> c) | c == a = [p]
        f _ = []

-- | Enumerating the observed nodes
observed :: Eq a => Network a -> [a]
observed n = filter (null . children n) . nodes $ n

-- | Enumerating the prior nodes
priors :: Eq a => Network a -> [a]
priors n = filter (null . parents n) . nodes $ n

-- | Enumerating the latent nodes
latent :: Eq a => Network a -> [a]
latent xs = nodes xs \\ (priors xs ++ observed xs)

-- | Index of a node
indexOf :: Eq a => Network a -> a -> Maybe a
indexOf xs a = msum (map f xs)
  where f ((p :@ i) :-> _) | p == a = Just i
        f _ = Nothing
