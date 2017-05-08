{-# LANGUAGE TypeFamilies, PolyKinds, GADTs, FlexibleInstances, ConstraintKinds #-}
module CT.Category.Singleton
	(
		Singleton(..)
	)
	where

import CT.Category

import Data.Kind

data Singleton (c :: kk -> Constraint) (a :: kk) (b :: kk) where
	Singleton :: (c a, a ~ b) => Singleton c a b

instance Category (Singleton c) where
	type Ob (Singleton c) = c
	id = Singleton
	Singleton . Singleton = Singleton

	observe Singleton x = x
