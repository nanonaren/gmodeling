module Statistics.GModeling.Models.LDA
  (
  ) where

import Statistics.GModeling.DSL

data LDALabels = Alpha | Beta | Topics | Topic
               | Doc | Symbols | Symbol

lda :: Network LDALabels
lda =
  [
    Only Alpha :-> Topics
  , Only Beta :-> Symbols
  , (Topics :@ [Doc]) :-> Topic
  , (Symbols :@ [Topic]) :-> Symbol
  ]
