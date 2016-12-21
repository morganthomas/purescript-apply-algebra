module Data.ApplyAlgebra where

import Control.Apply (class Apply, lift2)
import Data.Semigroup (class Semigroup, append)
import Data.Newtype (class Newtype)

newtype ApplyAlgebra f a = ApplyAlgebra (f a)

instance newtypeApplyAlgebra :: Newtype (ApplyAlgebra f a) (f a) where
  wrap x = ApplyAlgebra x
  unwrap (ApplyAlgebra x) = x

applyAlgebraLift2 :: forall f a b c. (f a -> f b -> f c) -> ApplyAlgebra f a -> ApplyAlgebra f b -> ApplyAlgebra f c
applyAlgebraLift2 f (ApplyAlgebra a) (ApplyAlgebra b) = ApplyAlgebra (f a b)

-- | An Apply applied to a Semigroup may give you a Semigroup. You need
-- | to check whether the axioms hold.
instance applySemigroup :: (Apply f, Semigroup a) => Semigroup (ApplyAlgebra f a) where
  append = applyAlgebraLift2 (lift2 append)
