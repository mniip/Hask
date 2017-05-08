{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module CT.Category.Monoidal.Hask
	(
	)
	where

import CT.Category.Hask
import CT.Category.Hask.Functors
import CT.Category.Monoidal
import CT.Category.Functor
import CT.NT

instance Monoidal (->) (,) () where
	assoc = \((x, y), z) -> (x, (y, z))
	coassoc = \(x, (y, z)) -> ((x, y), z)
	leftUnit = \x -> ((), x)
	coleftUnit = \((), x) -> x
	rightUnit = \x -> (x, ())
	corightUnit = \(x, ()) -> x

instance Monoidal (NT (->) (->)) (Compose (->) (->)) Identity where
