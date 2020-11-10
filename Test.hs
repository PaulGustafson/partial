module Test where

import qualified Partial as P
import qualified Data.Map as M

test_map = M.fromList [("alice",  23), ("bob", 32 :: Int)]
test_list = P.List $ take 100 $ repeat 100
test_comp = test_list P.. test_map
test_app = test_comp P.$ "bob"


