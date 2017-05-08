{-# LANGUAGE GADTs, PolyKinds, RankNTypes #-}

module CT.NT
	(
		NT(..)
	)
	where

import Prelude hiding (Functor(..))

import CT.Category
import CT.Functor

data NT (k :: kk -> kk -> *) (l :: ll -> ll -> *) (f :: kk -> ll) (g :: kk -> ll) where
	NT :: (Functor k l f, Functor k l g) => { runNT :: forall a. Ob k a => l (f a) (g a) } -> NT k l f g
