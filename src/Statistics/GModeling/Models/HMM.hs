module Statistics.GModeling.Models.HMM
  (
  ) where

import Statistics.GModeling.DSL

data HMMLabels = Alpha | Beta | Transition
               | Initial | Topic | Symbols | Symbol

hmm :: Network HMMLabels
hmm =
  [
    Only Alpha                      :-> Transition
  , Only Beta                       :-> Symbols
  , (Transition :@ [Initial,Topic]) :-> Topic
  , (Symbols :@ [Topic])            :-> Symbol
  ]
