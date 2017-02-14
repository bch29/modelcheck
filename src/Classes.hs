{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Classes where

import GHC.Exts (Constraint)

import Data.Map (Map)

-- | Represents a type of expressions which contains variables. These variables
-- can be substituted and used as expressions.
class HasVars v e | e -> v where
  -- | Creates an expression from a variable name.
  var :: v -> e

  {- |
  @
  subst var val expr
  @

  Substitutes all occurrences of the variable @var@ with the value @val@ in the
  expression @expr@.
  -}
  subst :: v -> e -> e -> e

-- | Describes a transition system with states in @s@ and transition labels in
-- @l@.
class TransitionSystem l s | s -> l where
  -- | Returns all possible transitions from the given state.
  transitions :: s -> Map l [s]

-- | Represents a type of models over transition systems which can be checked
-- for truth.
class Model l m | m -> l where
  -- | This can be used to e.g. make sure states are 'Ord' so they can be
  -- manipulated in a 'Data.Set.Set' or a 'Map'.
  type Checkable m s :: Constraint

  -- | Run the model checking algorithm.
  -- @checkModel prop process@ returns @True@ exactly when the CCS @process@
  -- satisfies @prop@.
  check :: (Checkable m s, TransitionSystem l s) => m s -> s -> Bool
