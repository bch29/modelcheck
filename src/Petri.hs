{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Petri where

import           Control.Monad    (guard)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (catMaybes)
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           TransitionSystem

data ConditionState = Marked | Unmarked
  deriving (Show, Eq, Ord)

data Petri e c =
  Petri
  { petriEvents  :: Map e (Set c, Set c)
    -- ^ Maps events to preconditions and postconditions (in that order). Also
    -- serves the purpose of describing exactly which events are present in the
    -- Petri net (the keys of this map).
  , petriMarkings  :: Map c ConditionState
    -- ^ Maps conditions to @{Marked, Unmarked}@. Also serves the purpose of
    -- describing exactly which conditions are present in the Petri net (the
    -- keys of this map).
  }
  deriving (Show, Eq, Ord)

instance (Ord c) => TransitionSystem e (Petri e c) where

  transitions petri =
    let events = petriEvents petri
        markings = petriMarkings petri

        transitionOn (preconditions, postconditions) = do
          let conditionValues conds = catMaybes [Map.lookup cond (petriMarkings petri) | cond <- Set.toList conds]
              preValues = conditionValues preconditions
              postValues = conditionValues postconditions

          guard $ all (== Marked) preValues && all (== Unmarked) postValues

          let newMarkings = Map.mapWithKey
                (\cond state ->
                    if Set.member cond preconditions
                    then Unmarked
                    else if Set.member cond postconditions
                         then Marked
                         else state) markings

          return [petri { petriMarkings = newMarkings }]

    in Map.mapMaybe transitionOn events
