{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prop
  ( Prop(..)
  , whenTrans
  , whenTransDot
  , nu
  , mu
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Classes
import CCS

data Prop v p l
  = Const (Set (CCS p l))
  | PTrue
  | PFalse
  | Not (Prop v p l)
  | Prop v p l :&&: Prop v p l
  | Prop v p l :||: Prop v p l
  | Trans (Label l) (Prop v p l)
  | TransDot (Prop v p l)
  | VarProp v
  | Nu v (Set (CCS p l)) (Prop v p l)
  deriving (Show, Eq, Ord)

whenTrans :: Label l -> Prop v p l -> Prop v p l
whenTrans lbl = Not . Trans lbl . Not

whenTransDot :: Prop v p l -> Prop v p l
whenTransDot = Not . TransDot . Not

nu :: v -> Prop v p l -> Prop v p l
nu v = Nu v Set.empty

mu :: v -> Prop v p l -> Prop v p l
mu v = Not . nu v . Not

instance (Eq v) => HasVars v (Prop v p l) where
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
