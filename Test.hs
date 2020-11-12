module Test where

import Partial 
import qualified Data.Map as M

test_map = M.fromList [("alice",  23), ("bob", 32 :: Int)]
test_list = take 100 $ repeat 100
test_comp = test_list .? test_map
test_app = test_comp $? "b" ++ "ob"



