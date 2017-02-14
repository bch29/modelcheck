{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Classes where

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
