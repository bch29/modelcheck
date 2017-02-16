{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Labels where

data Label l = Pos l | Neg l | Tau
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

class ToLabel l a where
  toLabel :: a -> l

instance ToLabel (Label Char) Char where
  toLabel = Pos

instance {-# OVERLAPPABLE #-} ToLabel l l where
  toLabel = id

opposite :: Label l -> Maybe (Label l)
opposite (Pos x) = Just (Neg x)
opposite (Neg x) = Just (Pos x)
opposite Tau     = Nothing

labelVal :: Label l -> Maybe l
labelVal (Pos l) = Just l
labelVal (Neg l) = Just l
labelVal Tau     = Nothing

isOpposite :: (Eq l) => Label l -> Label l -> Bool
isOpposite (Pos x) (Neg y) = x == y
isOpposite (Neg x) (Pos y) = x == y
isOpposite _ _             = False
