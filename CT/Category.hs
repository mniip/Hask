{-# LANGUAGE PolyKinds, TypeFamilies, RankNTypes, DefaultSignatures #-}

module CT.Category
	(
		Category(..)
	)
	where

import Data.Kind

class Category (k :: kk -> kk -> *) where
	type Ob k :: kk -> Constraint
	id :: Ob k a => k a a
	(.) :: k b c -> k a b -> k a c
	infixr 9 .

	observe :: k a b -> ((Ob k a, Ob k b) => r) -> r
	default observe :: k a b -> r -> r
	observe _ x = x

data Iso (k :: kk -> kk -> *) (a :: kk) (b :: kk) = Iso { withIso :: k a b, fromIso :: k b a }
