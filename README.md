# partial

A basic partial map typeclass.  Instances include Data.Map, List, Maybe a, and the usual partial function type "a -> Maybe b".

__Warning__: the composition operation (.?) is not always associative.  For example,

```
[a,b] .? ([0,999,1] .? [0,1]) = [a,b] .? [0,999] = [a]
([a,b] .? [0,999,1]) .? [0,1] = [a,b] .? [0,1] = [a,b]
```

Thanks to the following Reddit users for helpful suggestions and improvements:

* brandonchinn178
* Iceland_jack
* Tysonzero
* viercc
* dualized

Thread: https://www.reddit.com/r/haskell/comments/jry251/a_partial_function_typeclass_for_datamap_list_and/
