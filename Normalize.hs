{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
module Normalize where
import GHC.Generics
import Data.Traversable
    
class Normalize a where
    normalize :: a -> a
    default normalize :: (Generic a, GNormalize (Rep a)) => a -> a
    normalize x = to $ gnormalize (from x)
    
class GNormalize f where
    gnormalize :: f a -> f a
    
    
instance GNormalize U1 where
    gnormalize U1 = U1

instance (GNormalize a) => GNormalize (M1 i c a) where
  gnormalize = M1 . gnormalize . unM1

  
instance (Normalize a) => GNormalize (K1 i a) where
    gnormalize = K1 . normalize . unK1 

instance (GNormalize a, GNormalize b) => GNormalize (a :+: b) where
    gnormalize (L1 x) = L1 $ gnormalize x
    gnormalize (R1 x) = R1 $ gnormalize x

instance (GNormalize a, GNormalize b) => GNormalize (a :*: b) where
  gnormalize (a :*: b) = ((gnormalize a) :*: (gnormalize b))

   

    
