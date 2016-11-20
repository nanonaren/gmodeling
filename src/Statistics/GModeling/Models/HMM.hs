module Statistics.GModeling.Models.HMM
  (
  ) where

import Statistics.GModeling.DSL
import Statistics.GModeling.Gibbs
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

data HMM = Alpha | Beta | Transition
         | Topic | Symbols | Symbol

hmm :: Network HMM
hmm =
  [
    Only Alpha            :-> Transition
  , Only Beta             :-> Symbols
  , (Transition :@ Topic) :-> Topic
  , (Symbols :@ Topic)    :-> Symbol
  ]

type D = V.Vector (U.Vector (Int,Int))

reader :: D -> Reader HMM (Indexed HMM Int)
reader v = Reader
  {
    size = G.sum (G.map G.length v)
  , read idx k =
      let (i,j) = indices G.! idx
          (topic,symbol) = v G.! i G.! j
          (prev_topic,_) = v G.! i G.! (j-1)
      in case k of
           Topic -> Only topic
           Symbol -> Only symbol
           Symbols -> Topic :@ topic
           Transition -> if j==0
                         -- reserve Topic 0 for initial distribution
                         then Topic :@ 0
                         else Topic :@ prev_topic
  }
  where indices = U.fromList $ do
          i <- [0..G.length v-1]
          j <- [0..G.length (v G.! i)-1]
          return (i,j)
