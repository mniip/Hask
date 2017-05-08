{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module CT.Category.Hask
	(
		Always(..),
	)
	where

import CT.Category

class Always a
instance Always a

instance Category (->) where
	type Ob (->) = Always
	id = \x -> x
	p . q = \x -> p (q x)
