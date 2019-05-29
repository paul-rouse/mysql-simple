{-| Generic calculation of the "arity" of data-types.

This code was originally written by Li-yao Xia. See
<https://stackoverflow.com/a/56351505/1021134>.
-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeFamilies,
  UndecidableInstances, FlexibleContexts, DataKinds, TypeOperators,
  CPP #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Database.MySQL.Simple.Arity
  ( Arity
  , arity
#if MIN_VERSION_base(4,10,0)
, KnownNat
#endif
  ) where

import Numeric.Natural (Natural)
import Data.Proxy (Proxy)
#if MIN_VERSION_base(4,10,0)
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import GHC.Generics (M1, U1, K1, (:*:), Generic, Rep, (:+:), V1)
import GHC.TypeNats (KnownNat, Nat, type (+), natVal)
import GHC.TypeLits (TypeError, ErrorMessage(ShowType, Text, (:<>:)))

type family Arity (x :: Type) (f :: Type -> Type) :: Nat
type instance Arity x (M1 _ _ f) = Arity x f
type instance Arity x (f :*: g) = Arity x f + Arity x g
type instance Arity _ U1 = 0
type instance Arity _ (K1 i a) = 1
type instance Arity x (_ :+: _) =
 TypeError ('Text "Cannot calculate the arity of "
             ':<>: 'ShowType x
             ':<>: 'Text " because it has multiple constructors.")
type instance Arity x V1 =
  TypeError ('Text "Cannot calculate the arity of "
             ':<>: 'ShowType x
             ':<>: 'Text " because it has no constructors.")

-- We need the proxy argument to support GHC version prior to the
-- introduction of '-XTypeApplications'.
-- | @'arity' (Proxy :: a)@ for some type @a@ calculates the arity of
-- its only constructor.  @a@ *must* be a type with a single
-- constructor that has @n@ fields.
arity
  :: forall a
  .  Generic a
  => KnownNat (Arity a (Rep a))
  => Proxy a
  -> Natural
arity _ = natVal p
  where
  p :: Proxy (Arity a (Rep a))
  p = Proxy
#else
type family Arity (x :: *) (f :: * -> *) :: ()

arity :: Proxy a -> Natural
arity _ = 0
#endif
