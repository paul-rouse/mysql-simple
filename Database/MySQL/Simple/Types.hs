{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}

-- |
-- Module:      Database.MySQL.Simple.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Basic types.

module Database.MySQL.Simple.Types
    (
      Null(..)
    , Only(..)
    , Query(..)
    ) where

import Blaze.ByteString.Builder
import Control.Arrow
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

data Null = Null
          deriving (Read, Show, Typeable)

instance Eq Null where
    _ == _ = False
    _ /= _ = False

-- | A query string. This type is intended to make it difficult to
-- construct a SQL query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce SQL injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MySQL.Simple
-- >
-- > q :: Query
-- > q = "select ?"
newtype Query = Query {
      fromQuery :: ByteString
    } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . toByteString . Utf8.fromString

-- | A single-value collection.
--
-- This can be handy if you need to supply a single parameter to a SQL
-- query.
--
-- Example:
--
-- @query \"select x from scores where x > ?\" ('Only' (42::Int))@
newtype Only a = Only a
    deriving (Eq, Ord, Read, Show, NFData, Typeable, Functor)
