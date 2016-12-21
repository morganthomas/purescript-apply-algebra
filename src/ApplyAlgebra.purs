-- | This module lets you generate new algebras (semigroups, monoids, etc.) by applying
-- | applicative functors to existing algebras. An example use case: given a ring r,
-- | fixed-length vectors of r will again form a ring when the operations are defined
-- | coordinate-wise.
-- |
-- | None of these instances are correct in all possible applications;
-- | you always need to check when using an ApplyAlgebra typeclass instance whether
-- | the required algebraic axioms hold in your use case, or at least whether they hold
-- | in enough generality for your use case.
module Data.ApplyAlgebra where

import Data.Functor (map)
import Control.Apply (class Apply, lift2)
import Control.Applicative (class Applicative, pure)
import Data.Semigroup (class Semigroup, append)
import Data.Group (class Group, ginverse)
import Data.Monoid (class Monoid, mempty)
import Data.Semiring (class Semiring, one, zero, add, mul)
import Data.Ring (class Ring, sub)
import Data.CommutativeRing (class CommutativeRing)
import Data.Newtype (class Newtype)

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

-- | An Applicative applied to a Semiring may give you a Semiring. You need to
-- | check whether the axioms hold.
instance applySemiring :: (Applicative f, Semiring r) => Semiring (ApplyAlgebra f r) where
  one = ApplyAlgebra (pure one)
  zero = ApplyAlgebra (pure zero)
  add = applyAlgebraLift2 (lift2 add)
  mul = applyAlgebraLift2 (lift2 mul)

-- | An Applicative applied to a Ring may give you a Ring. You need to check
-- | whether the axioms hold.
instance applyRing :: (Applicative f, Ring r) => Ring (ApplyAlgebra f r) where
  sub = applyAlgebraLift2 (lift2 sub)

-- | An Applicative applied to a CommutativeRing may give you a CommutativeRing.
-- | You need to check whether the axioms hold.
instance applyCommutativeRing :: (Applicative f, CommutativeRing r) => CommutativeRing (ApplyAlgebra f r)
