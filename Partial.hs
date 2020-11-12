{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Partial where

import qualified Data.Map as M hiding (mapMaybe)
import Control.Monad
import Data.Witherable
import Control.Arrow

class (Filterable f) => Partial f where
  type Index f 
  
  ($?) :: f b -> Index f -> Maybe b
  infixl 4 $?

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

instance Filterable (Kleisli Maybe a) where
  mapMaybe g f = Kleisli (g <=< (runKleisli f))

instance Partial (Kleisli Maybe a) where
  type Index (Kleisli Maybe a) = a
  ($?) = runKleisli

