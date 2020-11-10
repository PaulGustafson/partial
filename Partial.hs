{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Partial where

import qualified Data.Map as M
import Control.Monad
import qualified Data.Maybe as D


class Partial f a where
  map :: (b -> c) -> f a b -> f a c
  mapMaybe  :: (b -> Prelude.Maybe c) -> f a b -> f a c
  ($) :: f a b -> a -> Prelude.Maybe b
  (.)  :: Partial g b => g b c -> f a b -> f a c
  g . f = Partial.mapMaybe (g Partial.$) f

instance (Ord k) => Partial M.Map k where
  map = M.map
  mapMaybe = M.mapMaybe
  ($) = flip M.lookup

newtype List a b = List { getList :: [b]} 
instance Partial Partial.List Int where
  map f xs = Partial.List (Prelude.map f (getList xs))
  mapMaybe f xs = Partial.List (D.mapMaybe f (getList xs))
  f $ n = if 0 <= n && n < length (getList f)
          then Just ((getList f) !! n)
          else Nothing
      

newtype Maybe a b = Maybe { getMaybe :: Prelude.Maybe b }
instance Partial Partial.Maybe () where
  map f x = Partial.Maybe (liftM f (getMaybe x))
  mapMaybe f x =  Partial.Maybe (f =<< (getMaybe x))
  x $ z = getMaybe x

newtype Fun a b = Fun { getFun :: a -> Prelude.Maybe b }
instance Partial Partial.Fun a where
  map g f  = Partial.Fun ((fmap g) Prelude.. (getFun f))
  mapMaybe g f  = Partial.Fun (g <=< (getFun f))
  ($) = getFun

