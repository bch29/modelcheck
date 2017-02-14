{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CCS
  (
    -- * Labels
    module Labels
    -- * CCS abstract syntax
  , CCS(..)
    -- * CCS construction combinators
  , (.=)
  , sigma
  , (.+.)
  , rec'
  , restrict
  , mapLabels
  , var
  ) where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Newtype

import TransitionSystem
import Labels
import Internal.HasVars

--------------------------------------------------------------------------------
--  CCS AST Datatype
--------------------------------------------------------------------------------

infixr 8 :->
infixr 5 :|:
infixr 4 .+.
infix 0 .=

-- | Represents a process in the pure CCS concurrent language. Contains
-- recursive variables of type @p@ and transition labels of type @l@.
data CCS p l
  = Nil
    -- ^ The @nil@ process, which never transitions.
  | Label l :-> CCS p l
    -- ^ @l ':->' p@ is a process which can transition on label @l@ to become a
    -- new process @p@. Corresponds with @l.p@ or @l -> p@.
  | Sigma (Map ChoiceLabel (CCS p l))
    -- ^ @'Sigma' ps@ is a nondeterministic choice of processes. Whenever some
    -- @p_i@ in @ps@ can transition to @p'@, this can also transition to @p'@.
    --
    -- Corresponds with @∑_I p_i@ where @I@ is the set of keys of @ps@ and the
    -- @p_i@ are the corresponding values.
  | CCS p l :|: CCS p l
    -- ^ Parallel composition. @p_1 ':|:' p_2@ corresponds with @p_1 || p_2@.
  | Restrict (Set l) (CCS p l)
    -- ^ Restriction.
  | Remap (Map l l) (CCS p l)
    -- ^ Remapping of labels.
  | Rec p (Map p (CCS p l))
    -- ^ Recursive definition of processes. @'Rec' p xs@ acts like the member of
    -- @xs@ with key @p@, with each of the keys of @xs@ bound as variables.
  | VarCCS p
    -- ^ Instantiate a recursive variable.
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
--  CCS construction combinators
--------------------------------------------------------------------------------

-- | Syntactic sugar for pairing, useful with 'sigma' and 'rec\''.
(.=) :: a -> b -> (a, b)
(.=) = (,)

-- | @'sigma' ps@ is a nondeterministic choice of processes. Whenever some @p_i@
-- in @ps@ can transition to @p'@, this can also transition to @p'@.
--
-- Corresponds with @∑_I p_i@ where @I@ is the set of keys of @ps@ and the @p_i@
-- are the corresponding values.
sigma :: [(ChoiceLabel, CCS p l)] -> CCS p l
sigma = Sigma . Map.fromList

-- | Nondeterministic choice between two processes. Equivalent to a 'sigma'
-- expression with only two options.
(.+.) :: CCS v l -> CCS v l -> CCS v l
p .+. q = sigma [0 .= p, 1 .= q]

-- | Recursive definition of processes. @'rec\'' p xs@ acts like the member of
-- @xs@ with key @p@, with each of the keys of @xs@ bound as variables.
rec' :: (Ord p) => p -> [(p, CCS p l)] -> CCS p l
rec' v = Rec v . Map.fromList

-- | Restriction.
restrict :: (Ord l) => [l] -> CCS p l -> CCS p l
restrict = Restrict . Set.fromList

-- | Map labels in the CCS expression to something else. Useful for reusing an
-- expression which has a different label type to the one we want. In order for
-- this to make sense the mapping function should be injective.
mapLabels :: Ord l' => (l -> l') -> CCS p l -> CCS p l'
mapLabels _ Nil = Nil
mapLabels f (lbl :-> process) = fmap f lbl :-> mapLabels f process
mapLabels f (Sigma processes) = Sigma (mapLabels f <$> processes)
mapLabels f (p1 :|: p2) = mapLabels f p1 :|: mapLabels f p2
mapLabels f (Restrict labels process) =
  Restrict (Set.map f labels) (mapLabels f process)
mapLabels f (Remap mapping process) =
  Remap (Map.mapKeys f . Map.map f $ mapping) (mapLabels f process)
mapLabels f (Rec v processes) =
  Rec v (mapLabels f <$> processes)
mapLabels _ (VarCCS v) = VarCCS v

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Ord p => HasVars p (CCS p l) where
  var = VarCCS

  subst v val (lbl :-> process) = lbl :-> subst v val process
  subst v val (Sigma processes) =
    Sigma (fmap (subst v val) processes)
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


instance (Ord l, Ord p) => TransitionSystem (Label l) (CCS p l) where
  transitions Nil = Map.empty
  transitions (lbl :-> process) = Map.singleton lbl [process]

  transitions (Sigma processes) =
    ala' UnionAppend foldMap
    (getMaybe . fmap transitions . (`Map.lookup` processes))
    (Map.keys processes)

  transitions (p :|: q) =
    let pTransitions = transitions p
        qTransitions = transitions q
        tauTransitions = Map.foldMapWithKey findTaus pTransitions
        findTaus l ps = getMaybe $ do
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

newtype UnionAppend k v = UnionAppend (Map k [v])

instance Newtype (UnionAppend k v) (Map k [v]) where
  pack = UnionAppend
  unpack (UnionAppend x) = x

instance Ord k => Monoid (UnionAppend k v) where
  mempty = pack mempty
  mappend x y = pack (Map.unionWith (++) (unpack x) (unpack y))

unionAppend :: (Ord k) => Map k [v] -> Map k [v] -> Map k [v]
unionAppend = Map.unionWith (++)

mapAll :: (a -> b) -> Map k [a] -> Map k [b]
mapAll = fmap . fmap

getMaybe :: (Monoid m) => Maybe m -> m
getMaybe = fromMaybe mempty
