{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, PolyKinds #-}

module CT.Limit
	(
		Limit(..), Colimit(..)
	)
	where

import CT.Category

class Limit (k :: kk -> kk -> *) (l :: ll -> ll -> *) (f :: kk -> ll) (a :: ll) | a -> f, f -> k l where
	project :: (Ob l a, Ob k b) => l a (f b)
	mediate :: Limit k l f b => l b a

class Colimit (k :: kk -> kk -> *) (l :: ll -> ll -> *) (f :: kk -> ll) (a :: ll) | a -> f, f -> k l where
	coproject :: (Ob l a, Ob k b) => l (f b) a
	comediate :: Colimit k l f b => l a b
