{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, PolyKinds, FlexibleContexts, UndecidableInstances, UndecidableSuperClasses #-}
module CT.Category.Monoidal
	(
		Monoidal(..)
	)
	where

import Prelude hiding (Functor(..))

import CT.Category
import CT.Functor
import CT.NT

class (Functor k (NT k k) t, Ob k i) => Monoidal (k :: kk -> kk -> *) (t :: kk -> kk -> kk) (i :: kk) | k -> t, t -> i where
	assoc :: (Ob k a, Ob k b, Ob k c) => k (t (t a b) c) (t a (t b c))
	coassoc :: (Ob k a, Ob k b, Ob k c) => k (t a (t b c)) (t (t a b) c)
	leftUnit :: Ob k a => k a (t i a)
	coleftUnit :: Ob k a => k (t i a) a
	rightUnit :: Ob k a => k a (t a i)
	corightUnit :: Ob k a => k (t a i) a
