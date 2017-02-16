{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Internal.HasVars where

-- | Represents a type of expressions which contains variables. These variables
-- can be substituted and used as expressions.
class HasVars v e | e -> v where
  -- | Creates an expression from a variable name.
  var :: v -> e

  {- |
  @subst var val expr@ substitutes all occurrences of the variable @var@ with
  the value @val@ in the expression @expr@.
  -}
  subst :: v -> e -> e -> e
