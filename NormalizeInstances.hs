{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module NormalizeInstances where
import Normalize
import Data.List

instance Normalize [Char] where
    normalize = id

--instance (Ord a, Normalize a) => Normalize [a] where
--    normalize xs = sort $ map normalize xs
    
instance Normalize Int where
    normalize = id
    
instance Normalize Char where
    normalize = id
    
instance (Normalize a, Normalize b) => Normalize (a, b)
instance (Normalize a, Normalize b, Normalize c) => Normalize (a, b, c)
instance (Normalize a, Normalize b, Normalize c, Normalize d) => Normalize (a, b, c, d)
instance (Normalize a, Normalize b, Normalize c, Normalize d, Normalize e) => Normalize (a, b, c, d, e)