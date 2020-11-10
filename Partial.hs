{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Partial where

import qualified Data.Map as M
import Control.Monad
import Data.Maybe


class Partial f a where
  map  :: (b -> Maybe c) -> f a b -> f a c
  ($) :: f a b -> a -> Maybe b
  (.)  :: Partial g b => g b c -> f a b -> f a c
  g . f = Partial.map (g Partial.$) f

instance (Ord k) => Partial M.Map k where
  map = M.mapMaybe
  ($) = flip M.lookup

newtype WList a b = WList { getList :: [b]} 
instance Partial WList Int where
  map f xs = WList (mapMaybe f (getList xs))
  f $ n = if 0 <= n && n < length (getList f)
          then Just ((getList f) !! n)
          else Nothing
         

newtype WMaybe a b = WMaybe { getMaybe :: Maybe b }
instance Partial WMaybe () where
  map f x =  WMaybe (f =<< (getMaybe x))
  x $ z = getMaybe x

newtype WFun a b = WFun { getFun :: a -> Maybe b }
instance Partial WFun a where
   map g f  = WFun (g <=< (getFun f))
   ($) = getFun

