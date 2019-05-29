{-| Generic calculation of the "arity" of data-types.

This code was originally written by Li-yao Xia. See
<https://stackoverflow.com/a/56351505/1021134>.
-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , ScopedTypeVariables
  , TypeFamilies
  , UndecidableInstances
  , FlexibleContexts
  , DataKinds
  , TypeOperators
#-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Database.MySQL.Simple.Arity
  ( Arity
  , arity
  , KnownNat
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Numeric.Natural (Natural)
import GHC.Generics (M1, U1, K1, (:*:), Generic, Rep)
import GHC.TypeNats (KnownNat, Nat, type (+), natVal)

type family Arity (f :: Type -> Type) :: Nat
type instance Arity (M1 _ _ f) = Arity f
type instance Arity (f :*: g) = Arity f + Arity g
type instance Arity U1 = 0
type instance Arity (K1 i a) = 1

-- We need the proxy argument to support GHC version prior to the
-- introduction of '-XTypeApplications'.
-- | @'arity' (Proxy :: a)@ for some type @a@ calculates the arity of
-- its only constructor.  @a@ *must* be a type with a single
-- constructor that has @n@ fields.
arity
  :: forall a
  .  Generic a
  => KnownNat (Arity (Rep a))
  => Proxy a
  -> Natural
arity _ = natVal p
  where
  p :: Proxy (Arity (Rep a))
  p = Proxy
