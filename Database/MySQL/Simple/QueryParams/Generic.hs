{-| Generic deriviation of 'Database.MySQL.Simple.QueryParams.QueryParams'. -}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, InstanceSigs #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Database.MySQL.Simple.QueryParams.Generic
  ( QueryParams(..)
  ) where

import Prelude ()
import Database.MySQL.Simple.Prelude
import GHC.Generics

import Database.MySQL.Simple.Param (Action(..), Param(..))

class QueryParams f where
  renderParams :: f a -> [Action]

-- This instance might not make sense, though the signature of
-- 'renderParams' sort of implies that it does since it returns a
-- '[]''s in stead of a 'NonEmpty''s.
instance QueryParams U1 where
  renderParams = const mempty

instance Param a => QueryParams (K1 i a) where
  renderParams (K1 a) = pure $ render a

instance QueryParams a => QueryParams (M1 i c a) where
  renderParams (M1 a) = renderParams a

instance (QueryParams a, QueryParams b) => QueryParams (a :*: b) where
  renderParams (a :*: b) = renderParams a <> renderParams b