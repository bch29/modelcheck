{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Labels where

type ChoiceLabel = Int

data Label l = Pos l | Neg l | Tau
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

opposite :: Label l -> Maybe (Label l)
opposite (Pos x) = Just (Neg x)
opposite (Neg x) = Just (Pos x)
opposite Tau = Nothing

labelVal :: Label l -> Maybe l
labelVal (Pos l) = Just l
labelVal (Neg l) = Just l
labelVal Tau = Nothing

isOpposite :: (Eq l) => Label l -> Label l -> Bool
isOpposite (Pos x) (Neg y) = x == y
isOpposite (Neg x) (Pos y) = x == y
isOpposite _ _ = False
