{-# LANGUAGE PolyKinds, MultiParamTypeClasses, FunctionalDependencies, RankNTypes, DefaultSignatures #-}

module CT.Functor
	(
		Functor(..)
	)
	where

import Prelude hiding (Functor(..))

import CT.Category

class Functor (k :: kk -> kk -> *) (l :: ll -> ll -> *) (f :: kk -> ll) | f -> k l where
	fmap :: k a b -> l (f a) (f b)
