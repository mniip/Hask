{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, PolyKinds, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module CT.Category.Hask.Functors
	(
		Identity(..), runIdentity,
		Compose(..), getCompose
	)
	where

import Prelude hiding (Functor(..), (.), id)

import CT.Category
import CT.Category.Hask
import CT.Category.Op
import CT.Category.Functor
import CT.Functor
import CT.NT

data Identity a = Identity a deriving Show
runIdentity (Identity x) = x

instance Functor (->) (->) Identity where fmap f = \(Identity x) -> Identity (f x)

data Compose (k :: kk -> kk -> *) (l :: ll -> ll -> *) (f :: ll -> *) (g :: kk -> ll) (a :: kk) where
	Compose :: f (g a) -> Compose k l f g a
getCompose :: Compose k l f g a -> f (g a)
getCompose (Compose x) = x

instance (Functor l (->) f, Functor k l g) => Functor k (->) (Compose k l f g) where fmap f = \(Compose x) -> Compose (fmap (fmap f) x)
instance (Functor l (->) f) => Functor (NT k l) (NT k (->)) (Compose k l f) where fmap (NT f) = NT $ \(Compose x) -> Compose $ fmap f x
instance (Category k, Category l) => Functor (NT l (->)) (NT (NT k l) (NT k (->))) (Compose k l) where
	fmap :: NT l (->) f g -> NT (NT k l) (NT k (->)) (Compose k l f) (Compose k l g)
	fmap (NT f) = NT $ NT $ (undefined :: p f h a -> p g h a)

instance Functor (->) (->) ((->) a) where fmap f = \g -> f . g
instance Functor (Op (->)) (NT (->) (->)) (->) where fmap (Op f) = NT $ \g -> g . f

instance Functor (->) (->) ((,) a) where fmap f = \(x, y) -> (x, f y)
instance Functor (->) (NT (->) (->)) (,) where fmap f = NT $ \(x, y) -> (f x, y)
