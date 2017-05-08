{-# LANGUAGE PolyKinds, DataKinds, GADTs, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RankNTypes, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}

module CT.Limit.Product
	(
	)
	where

import Prelude hiding (id, (.), Functor(..), fst, snd)

import CT.Category
import CT.Category.Hask
import CT.Category.Singleton
import CT.Functor
import CT.Limit
import CT.NT

import Data.Proxy

class KnownBool (a :: Bool) where observeBool :: p a -> ((a ~ False) => r) -> ((a ~ True) => r) -> r
instance KnownBool False where observeBool _ x _ = x
instance KnownBool True where observeBool _ _ x = x

data BoolDiag (a :: *) (b :: *) (k :: Bool) where
	Fst :: { getFst :: a } -> BoolDiag a b False
	Snd :: { getSnd :: b } -> BoolDiag a b True

data AProduct (k :: kk -> kk -> *) (f :: kk -> kk -> kk) (a :: kk) (b :: kk) where
	AProduct :: Limit (Singleton KnownBool) k (ProductDiag f a b) (f a b) => AProduct k f a b

class Product (k :: kk -> kk -> *) (f :: kk -> kk -> kk) where
	type family ProductDiag f :: kk -> kk -> Bool -> *
	fst :: k (f a b) a
	snd :: k (f a b) b
	mediateProduct :: (Ob k a, Ob k b, ProductDiag f ~ ProductDiag g, Product k g) => k (g a b) (f a b)
	mediateProduct = let result = case pick result of (AProduct, AProduct) -> mediate in result
		where
			pick :: (Ob k a, Ob k b, Product k f, Product k g) => k (g a b) (f a b) -> (AProduct k f a b, AProduct k g a b)
			pick _ = (observeProduct, observeProduct)
	observeProduct :: (Ob k a, Ob k b) => AProduct k f a b

data ACoproduct (k :: kk -> kk -> *) (f :: kk -> kk -> kk) (a :: kk) (b :: kk) where
	ACoproduct :: Colimit (Singleton KnownBool) k (CoproductDiag f a b) (f a b) => ACoproduct k f a b

class Coproduct (k :: kk -> kk -> *) (f :: kk -> kk -> kk) where
	type family CoproductDiag f :: kk -> kk -> Bool -> *
	left :: k a (f a b)
	right :: k b (f a b)
	comediateCoproduct :: (CoproductDiag f ~ CoproductDiag g, Coproduct k g) => k (f a b) (g a b)
	comediateCoproduct = let result = case pick result of (ACoproduct, ACoproduct) -> comediate in result
		where
			pick :: (Coproduct k f, Coproduct k g) => k (f a b) (g a b) -> (ACoproduct k f a b, ACoproduct k g a b)
			pick _ = (observeCoproduct, observeCoproduct)
	observeCoproduct :: ACoproduct k f a b

instance (a ~ a', b ~ b') => Limit (Singleton KnownBool) (->) (BoolDiag a b) (a', b') where
	project (x, y) = let result = observeBool result (Fst x) (Snd y) in result
	mediate x = (getFst (project x), getSnd (project x))

instance Product (->) (,) where
	type ProductDiag (,) = BoolDiag
	fst = getFst . project
	snd = getSnd . project
	observeProduct = AProduct

instance (a ~ a', b ~ b') => Colimit (Singleton KnownBool) (->) (BoolDiag a b) (Either a' b') where
	coproject (Fst x) = Left x
	coproject (Snd x) = Right x
	comediate (Left x) = coproject (Fst x)
	comediate (Right x) = coproject (Snd x)

instance Coproduct (->) Either where
	type CoproductDiag Either = BoolDiag
	left = coproject . Fst
	right = coproject . Snd
	observeCoproduct = ACoproduct
