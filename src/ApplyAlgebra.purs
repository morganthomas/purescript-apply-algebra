module Data.ApplyAlgebra where

import Data.Functor (map)
import Control.Apply (class Apply, lift2)
import Control.Applicative (class Applicative, pure)
import Data.Semigroup (class Semigroup, append)
import Data.Newtype (class Newtype)
import Data.Group (class Group, ginverse)
import Data.Monoid (class Monoid, mempty)

newtype ApplyAlgebra f a = ApplyAlgebra (f a)

instance newtypeApplyAlgebra :: Newtype (ApplyAlgebra f a) (f a) where
  wrap x = ApplyAlgebra x
  unwrap (ApplyAlgebra x) = x

applyAlgebraLift :: forall f a b. (f a -> f b) -> ApplyAlgebra f a -> ApplyAlgebra f b
applyAlgebraLift f (ApplyAlgebra a) = ApplyAlgebra (f a)

applyAlgebraLift2 :: forall f a b c. (f a -> f b -> f c) -> ApplyAlgebra f a -> ApplyAlgebra f b -> ApplyAlgebra f c
applyAlgebraLift2 f (ApplyAlgebra a) (ApplyAlgebra b) = ApplyAlgebra (f a b)

-- | An Apply applied to a Semigroup may give you a Semigroup. You need
-- | to check whether the axioms hold.
instance applySemigroup :: (Apply f, Semigroup a) => Semigroup (ApplyAlgebra f a) where
  append = applyAlgebraLift2 (lift2 append)

-- | An Applicative applied to a Monoid may give you a Monoid. You need
-- | to check whether the axioms hold.
instance applyMonoid :: (Applicative f, Monoid a) => Monoid (ApplyAlgebra f a) where
  mempty = ApplyAlgebra (pure mempty)

-- | An Applicative applied to a Group may give you a Group. You need to
-- | check whether the axioms hold. Just a Functor suffices for ginverse,
-- | but you also need to be able to instantiate applyMonoid and applySemigroup,
-- | which requires an Applicative.
instance applyGroup :: (Applicative f, Group a) => Group (ApplyAlgebra f a) where
  ginverse = applyAlgebraLift (map ginverse)
