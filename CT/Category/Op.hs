{-# LANGUAGE PolyKinds, TypeFamilies #-}
module CT.Category.Op
	(
		Op(..)
	)
	where

import Prelude hiding (id, (.))

import CT.Category

data Op (k :: kk -> kk -> *) (a :: kk) (b :: kk) = Op { getOp :: k b a }

instance Category k => Category (Op k) where
	type Ob (Op k) = Ob k
	id = Op id
	Op f . Op g = Op (g . f)

	observe (Op f) x = observe f x
