module Statistics.GModeling.Models.HMM
  (
  ) where

import Statistics.GModeling.DSL
import Statistics.GModeling.Gibbs
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Mutable as MU

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

reader :: D -> Reader HMM Int
reader v = Reader
  {
    size = G.sum (G.map G.length v)
  , readn = \idx k ->
      let (i,j) = indices G.! idx
          (topic,symbol) = v G.! i G.! j
          (prev_topic,_) = v G.! i G.! (j-1)
      in case k of
           Topic -> topic
           Symbol -> symbol
           Symbols -> topic
           Transition -> if j==0
                         -- reserve Topic 0 for initial distribution
                         then 0
                         else prev_topic
  , copy = do
      m <- G.mapM U.thaw v
      return Writer
        {
          writen = \idx k v -> do
            let (i,j) = indices G.! idx
            case k of
              Topic -> do
                (_,s) <- MU.unsafeRead (m G.! i) j
                MU.unsafeWrite (m G.! i) j (v,s)
        , readOnly = G.mapM U.freeze m >>= return . reader
        }
  }
  where indices = U.fromList $ do
          i <- [0..G.length v-1]
          j <- [0..G.length (v G.! i)-1]
          return (i,j)
