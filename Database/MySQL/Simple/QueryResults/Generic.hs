{-# LANGUAGE ScopedTypeVariables, TypeOperators, InstanceSigs, KindSignatures #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Database.MySQL.Simple.QueryResults.Generic
  ( QueryResults(..)
  ) where

import Prelude ()
import Database.MySQL.Simple.Prelude
import Data.ByteString (ByteString)
import GHC.Generics
import Control.Exception (throw)

import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple.Result
  (Result(convert), ResultError(..))

class QueryResults f where
  convertResults :: [Field] -> [Maybe ByteString] -> (f a)

-- This instance might not make sense, though the signature of
-- 'convertResults' sort of implies that it does since it takes in two
-- '[]''s in stead of two 'NonEmpty''s.
instance QueryResults U1 where
  convertResults xs ys = case zip xs ys of
    [] -> U1
    _  -> genericConversionError

instance Result a => QueryResults (K1 i a) where
  convertResults xs ys = case zip xs ys of
    [(x, y)] -> K1 $ convert x y
    _ -> genericConversionError

instance QueryResults a => QueryResults (M1 i c a) where
  convertResults xs ys =
    M1 $ convertResults xs ys

instance (QueryResults a, QueryResults b) => QueryResults (a :*: b) where
  convertResults :: forall x . [Field] -> [Maybe ByteString] -> (:*:) a b x
  convertResults (x:xs) (y:ys) =
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
    convertResults [x] [y] :*: convertResults xs ys
  convertResults _ _ = genericConversionError
    where

genericConversionError :: a
genericConversionError = throw $ ConversionFailed
  { errSQLType = cantDetermine
  , errHaskellType = cantDetermine
  , errFieldName = cantDetermine
  , errMessage = msg
  }
  where
  msg
    =  "Database.MySQL.Simple.QueryResults.Generic.convertResult: "
    <> "Mis-match between result and target-type."
  cantDetermine = "Cannot be determined."
