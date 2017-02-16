{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Prop
  (
    -- * Model checking
    module Model
    -- * Proposition abstract syntax
  , Prop(..)
    -- * Proposition construction combinators
  , whenTrans
  , whenTransDot
  , nu
  , mu
  , HasVars(..)
  ) where

import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Internal.HasVars
import           Model
import           TransitionSystem

-- | A proposition over transition systems in the modal μ-calculus. Contains
-- variables of type @v@ and transition labels of type @l@. Acts on a transition
-- system which has states of type @s@.
data Prop v l s
  = Const (Set s)
    -- ^ Succeeds when given exactly those process states in the given set.
    --
    -- @'Const' ('Set.fromList' [p_1, ..., p_k])@ corresponds with
    -- @{p_1, ..., p_2}@
  | PTrue
    -- ^ Always succeeds.
    --
    -- Corresponds with @T@.
  | PFalse
    -- ^ Always fails.
    --
    -- Corresponds with @F@.
  | Not (Prop v l s)
    -- ^ Succeeds exactly when the given proposition fails.
    --
    -- @'Not' A@ corresponds with @¬ A@.
  | Prop v l s :&&: Prop v l s
    -- ^ Succeeds when both of the arguments succeed.
    --
    -- @A ':&&:' B@ corresponds with @A ∧ B@.
  | Prop v l s :||: Prop v l s
    -- ^ Succeeds when either of the arguments succeeds.
    --
    -- @A ':||:' B@ corresponds with @A ∨ B@.
  | Trans l (Prop v l s)
    -- ^ Succeeds when the state can transition with the given label, then the
    -- given proposition also succeeds.
    --
    -- @'Trans' l A@ corresponds with @\<l\> A@.
  | TransDot (Prop v l s)
    -- ^ Succeeds when the state can transition with any label, then the given
    -- proposition also succeeds.
    --
    -- @'TransDot' A@ corresponds with @\<.\> A@.
  | VarProp v
    -- ^ References a variable bound by another constructor, such as 'Nu'.
  | Nu v (Set s) (Prop v l s)
    -- ^ @'Nu' X ('Set.fromList' [p_1, ..., p_k]) A@ takes the greatest fixed
    -- point of the expression @A@, with @X@ bound recursively in @A@. Also
    -- matches any process in the given set.
    --
    -- Corresponds with @ν X {p_1, ..., p_k} A@.
  deriving (Show, Eq, Ord)

-- | Succeeds if, whenever the state can transition with the given label, then
-- the given proposition also succeeds.
--
-- @'whenTrans' l A@ corresponds with @[l] A@.
whenTrans :: l -> Prop v l s -> Prop v l s
whenTrans lbl = Not . Trans lbl . Not

-- | Succeeds if, whenever the state can transition with any label, then the
-- given proposition also succeeds.
--
-- @'whenTransDot' A@ corresponds with @[.] A@.
whenTransDot :: Prop v l s -> Prop v l s
whenTransDot = Not . TransDot . Not

-- | @'nu' X A@ takes the greatest fixed point of the expression @A@, with @X@
-- bound recursively in @A@.
--
-- Corresponds with @ν X. A@.
nu :: v -> Prop v l s -> Prop v l s
nu v = Nu v Set.empty

-- | @'mu' X A@ takes the least fixed point of the expression @A@, with @X@
-- bound recursively in @A@.
--
-- Corresponds with @μ X. A@.
mu :: v -> Prop v l s -> Prop v l s
mu v = Not . nu v . Not

instance (Eq v) => HasVars v (Prop v l s) where
  var = VarProp

  subst v val (Not prop) = Not (subst v val prop)
  subst v val (p1 :&&: p2) = subst v val p1 :&&: subst v val p2
  subst v val (p1 :||: p2) = subst v val p1 :||: subst v val p2
  subst v val (Trans lbl prop) = Trans lbl (subst v val prop)
  subst v val (TransDot prop) = TransDot (subst v val prop)
  subst v val (VarProp v')
    | v == v' = val
    | otherwise = var v'
  subst v val prop'@(Nu nuV processes prop)
    | v == nuV = prop' -- Don't substitute bound variables
    | otherwise = Nu nuV processes (subst v val prop)
  subst _ _ prop = prop

instance (Eq v, Ord l) => Model l (Prop v l) where
  type Checkable (Prop v l) s = Ord s

  check (Const set) process = Set.member process set
  check PTrue _ = True
  check PFalse _ = False
  check (VarProp _) _ = False
  check (Not prop) process = not (check prop process)
  check (prop1 :&&: prop2) process = check prop1 process && check prop2 process
  check (prop1 :||: prop2) process = check prop1 process || check prop2 process

  check (Trans lbl prop) process =
    let allTransitions = transitions process
        lblTransitions = fromMaybe [] (Map.lookup lbl allTransitions)
    in any (check prop) lblTransitions

  check (TransDot prop) process =
    let allTransitions = Map.foldr (++) [] (transitions process)
    in any (check prop) allTransitions

  check (Nu v processes prop) process
    | Set.member process processes = True
    | otherwise =
      let prop' = Nu v (Set.insert process processes) prop
      in check (subst v prop' prop) process
