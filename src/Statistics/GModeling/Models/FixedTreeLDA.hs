module Statistics.GModeling.Models.FixedTreeLDA
  (
  ) where

import Statistics.GModeling.DSL

data LDATreeLabels =
    Alpha1 | Alpha2 | Beta
  | TopicDepth | TopicPath | Topic | Doc
  | Symbols | Symbol

ldaTree :: Network LDATreeLabels
ldaTree =
  [
    Only Alpha1         :-> TopicDepth
  , Only Alpha2         :-> TopicPath
  , Only Beta           :-> Symbols
  , (TopicPath :@ Doc)  :-> Topic
  , (TopicDepth :@ Doc) :-> Topic
  , (Symbols :@ Topic)  :-> Symbol
  ]
