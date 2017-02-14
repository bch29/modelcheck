{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module TransitionSystem where

import           Data.Map (Map)

-- | Describes a transition system with states in @s@ and transition labels in
-- @l@.
class TransitionSystem l s | s -> l where
  -- | Returns all possible transitions from the given state.
  transitions :: s -> Map l [s]
