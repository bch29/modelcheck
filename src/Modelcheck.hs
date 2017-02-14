module Modelcheck
  ( module Prop
  , module CCS
  , module Classes
  , check
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Prop
import CCS
import Classes

-- | Run the model checking algorithm.
-- @check prop process@ returns @True@ if the CCS @process@ satisfies @prop@.
check :: (Ord p, Ord l, Eq v) => Prop v p l -> CCS p l -> Bool
check (Const set) process = Set.member process set
check PTrue _ = True
check PFalse _ = False
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

check (VarProp _) _ = False

check (Nu v processes prop) process
  | Set.member process processes = True
  | otherwise =
    let prop' = Nu v (Set.insert process processes) prop
    in check (subst v prop' prop) process
