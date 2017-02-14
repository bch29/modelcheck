{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CCS
  ( module Labels
  , CCS(..)
  , (.+.)
  , transitions
  ) where

import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Classes
import Labels

--------------------------------------------------------------------------------
--  CCS AST Datatype
--------------------------------------------------------------------------------

infixr 4 :->
infixr 5 :|:

data CCS p l
  = Nil
  | Label l :-> CCS p l
  | Sigma (Set ChoiceLabel) (Map ChoiceLabel (CCS p l))
  | CCS p l :|: CCS p l
  | Restrict (Set l) (CCS p l)
  | Remap (Map l l) (CCS p l)
  | Rec p (Map p (CCS p l))
  | VarCCS p
  deriving (Show, Eq, Ord)

(.+.) :: CCS v l -> CCS v l -> CCS v l
p .+. q =
  Sigma (Set.fromList [0, 1])
        (Map.fromList [(0, p), (1, q)])

instance Ord p => HasVars p (CCS p l) where
  var = VarCCS

  subst v val (lbl :-> process) = lbl :-> subst v val process
  subst v val (Sigma choices processes) =
    Sigma choices (fmap (subst v val) processes)
  subst v val (p1 :|: p2) = subst v val p1 :|: subst v val p2
  subst v val (Restrict labels process) =
    Restrict labels (subst v val process)
  subst v val (Remap mapping process) =
    Remap mapping (subst v val process)
  subst v val (VarCCS v') | v == v' = val
                          | otherwise = var v'
  subst v val (Rec recv clauses) =
    -- Every viable used for recursion is bound in a `Rec` expression, so don't
    -- substitute any of them!
    if not (Set.member v (Map.keysSet clauses))
    then Rec recv (fmap (subst v val) clauses)
    else Rec recv clauses
  subst _ _ process = process


--------------------------------------------------------------------------------
--  Substitutions and Transitions
--------------------------------------------------------------------------------

transitions :: (Ord l, Ord p) => CCS p l -> Map (Label l) [CCS p l]
transitions Nil = Map.empty
transitions (lbl :-> process) = Map.singleton lbl [process]

transitions (Sigma choices processes) = Set.foldr step Map.empty choices
  where
    step choice accum = fromMaybe accum $ do
      p <- Map.lookup choice processes
      let ps = transitions p
      return (ps `unionAppend` accum)

transitions (p :|: q) =
  let pTransitions = transitions p
      qTransitions = transitions q
      tauTransitions = Map.foldMapWithKey findTaus pTransitions
      findTaus l ps = fromMaybe [] $ do
        l' <- opposite l
        qs <- Map.lookup l' qTransitions
        return [p' :|: q' | p' <- ps, q' <- qs]
  in mapAll (:|: q) pTransitions `unionAppend`
     mapAll (p :|:) qTransitions `unionAppend`
     Map.singleton Tau tauTransitions

transitions (Restrict labels process) =
  Map.filterWithKey
    (\lbl _ -> maybe True (\l -> not (Set.member l labels)) (labelVal lbl))
    (transitions process)

transitions (Remap mapping process) =
  Map.mapKeysWith (++) remap (transitions process)
  where
    remap lbl = fromMaybe lbl (traverse (`Map.lookup` mapping) lbl)

transitions (VarCCS _) = Map.empty

transitions (Rec v processes)
  | Just z <- Map.lookup v processes =
      let step v' = subst v' (Rec v' processes)
      in transitions (foldr step z $ Map.keys processes)
  | otherwise = Map.empty

--------------------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------------------

unionAppend :: (Ord k) => Map k [v] -> Map k [v] -> Map k [v]
unionAppend = Map.unionWith (++)

mapAll :: (a -> b) -> Map k [a] -> Map k [b]
mapAll = fmap . fmap
