{-# LANGUAGE TypeFamilies #-}

module CT.Category.Functor
	(
	)
	where

import Prelude hiding (id, (.), Functor(..))

import CT.Category
import CT.Functor
import CT.NT

instance (Category k, Category l) => Category (NT k l) where
	type Ob (NT k l) = Functor k l
	id = NT (fmap id)
	NT f . NT g = NT $ f . g

	observe (NT f) x = x
