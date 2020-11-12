{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Partial where

import qualified Data.Map as M hiding (mapMaybe)
import Control.Monad
import Data.Witherable


class (Filterable f) => Partial f where
  type Index f   -- thanks to /u/dualized
  
  ($?) :: f b -> Index f -> Maybe b
  infixl 4 $?

  -- Warning: not always associative! (thanks /u/viercc)
  -- [a,b] .? ([0,999,1] .? [0,1]) = [a,b] .? [0,999] = [a]
  -- ([a,b] .? [0,999,1]) .? [0,1] = [a,b] .? [0,1] = [a,b]
  (.?)  :: Partial g => g c -> f (Index g) -> f c
  g .? f = mapMaybe (g $?) f

instance (Ord k) => Partial (M.Map k) where
  type Index (M.Map k) = k
  ($?) = flip M.lookup

instance Partial [] where
  type Index [] = Int
  f $? n = if 0 <= n && n < length f then Just (f !! n) else Nothing
      
instance Partial Maybe where
  type Index Maybe = ()
  ($?) = const


-- same as Kleisli Maybe a, but different Functor instance (I think)
newtype PFun a b = PFun { getPFun :: a -> Maybe b}

instance Functor (PFun a) where
  fmap g f  = PFun ((fmap g) . (getPFun f))

instance Filterable (PFun a) where
  mapMaybe g f = PFun (g <=< (getPFun f))

instance Partial (PFun a) where
  type Index (PFun a) = a
  ($?) = getPFun

