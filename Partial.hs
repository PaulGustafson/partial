{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Partial where

import qualified Data.Map as M
import Control.Monad
import qualified Data.Maybe as D
import Data.Functor.Compose

class Partial f a where
  ($?) :: f a b -> a -> Maybe b
  infixl 4 $?

  mapMaybe  :: (b -> Maybe c) -> f a b -> f a c
    
  mapP :: (b -> c) -> f a b -> f a c
  mapP f = Partial.mapMaybe (Just . f)

  (.?)  :: Partial g b => g b c -> f a b -> f a c
  g .? f = Partial.mapMaybe (g $?) f

instance (Ord k) => Partial M.Map k where
  mapP = M.map
  mapMaybe = M.mapMaybe
  ($?) = flip M.lookup

newtype PList a b = PList { getPList :: [b]} 
instance Partial PList Int where
  mapP f xs = PList (Prelude.map f (getPList xs))
  mapMaybe f xs = PList (D.mapMaybe f (getPList xs))
  f $? n = if 0 <= n && n < length (getPList f)
          then Just ((getPList f) !! n)
          else Nothing
      
newtype PMaybe a b = PMaybe { getPMaybe :: Maybe b }
instance Partial PMaybe () where
  mapP f x = PMaybe (liftM f (getPMaybe x))
  mapMaybe f x =  PMaybe (f =<< (getPMaybe x))
  x $? z = getPMaybe x

newtype PFun a b = PFun { getPFun :: a -> Maybe b }
instance Partial PFun a where
  mapP g f  = PFun ((fmap g) . (getPFun f))
  mapMaybe g f  = PFun (g <=< (getPFun f))
  ($?) = getPFun

-- thanks to /u/brandonchinn178
instance Partial f a => Functor (f a) where
  fmap = mapP

-- https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html
class (Monad m, Functor f) => KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b

instance Partial f a => KleisliFunctor Maybe (f a) where
  kmap = mapMaybe

