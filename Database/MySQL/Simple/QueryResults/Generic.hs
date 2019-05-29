{-| Generic deriviation of 'Database.MySQL.Simple.QueryResults.QueryResults'. -}
{-# LANGUAGE
    ScopedTypeVariables
  , TypeOperators
  , InstanceSigs
  , FlexibleContexts
  , AllowAmbiguousTypes
  #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Database.MySQL.Simple.QueryResults.Generic
  ( QueryResults(..)
  , convert
  ) where

import Prelude ()
import Database.MySQL.Simple.Prelude
import Data.ByteString (ByteString)
import GHC.Generics
import qualified GHC.Generics as Generics
import Control.Exception (throw)

import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple.Result
  (Result, ResultError)
import qualified Database.MySQL.Simple.Result as Result
import Data.Proxy (Proxy(Proxy))
import Database.MySQL.Simple.Arity (Arity, arity, KnownNat)

-- | Generic implementation of
-- 'Database.MySQL.Simple.QueryResults.Generic.convertResults'.
convert
  :: forall a
  .  Generic a
  => KnownNat (Arity (Rep a))
  => QueryResults (Rep a)
  => [Field]
  -> [Maybe ByteString]
  -> a
convert xs ys = Generics.to $ convertResults err xs ys
  where
  err = Result.convertException xs ys $ fromIntegral $ arity (Proxy :: Proxy a)

-- | The generic counterpart to 'Database.MySQL.Simple.QueryResults.QueryResults'.
class QueryResults f where
  convertResults :: ResultError -> [Field] -> [Maybe ByteString] -> f a

-- This instance might not make sense, though the signature of
-- 'convertResults' sort of implies that it does since it takes in two
-- '[]''s in stead of two 'NonEmpty''s.
instance QueryResults U1 where
  convertResults err xs ys = case zip xs ys of
    [] -> U1
    _  -> throw err

instance Result a => QueryResults (K1 i a) where
  convertResults err xs ys = case zip xs ys of
    [(x, y)] -> K1 $ Result.convert x y
    _ -> throw err

instance QueryResults a => QueryResults (M1 i c a) where
  convertResults err xs ys =
    M1 $ convertResults err xs ys

instance (QueryResults a, QueryResults b) => QueryResults (a :*: b) where
  convertResults
    :: forall x
    .  ResultError
    -> [Field]
    -> [Maybe ByteString]
    -> (:*:) a b x
  convertResults err (x:xs) (y:ys) =
    -- I'm concerned about this implementation since it's biased
    -- towards one side.  Meaning that it's expecting the chain of
    -- ':*:' to lean to the right.  As an example of the problem
    -- consider the following:
    --
    --     > data T = T Int Int Int deriving (Generic)
    --     > from (T 1 2 3)
    --     M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1}} :*: (M1 {unM1 = K1 {unK1 = 2}} :*: M1 {unM1 = K1 {unK1 = 3}})}}
    --
    -- If the result in stead had been like this (note the re-bracketing):
    --
    --     M1 {unM1 = (M1 {unM1 = M1 {unM1 = K1 {unK1 = 1}} :*: M1 {unM1 = K1 {unK1 = 2}}}) :*: M1 {unM1 = K1 {unK1 = 3}}}
    --
    -- Then the generic derivation for 'T' would fail.
    convertResults err [x] [y] :*: convertResults err xs ys
  convertResults err _ _ = throw err
