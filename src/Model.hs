{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Model where

import           GHC.Exts         (Constraint)

import           TransitionSystem

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
