{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}

-- |
-- Module:      Database.MySQL.Simple.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- Basic types.

module Database.MySQL.Simple.Types
    (
      Null(..)
    , Only(..)
    , In(..)
    , VaArgs(..)
    , Binary(..)
    , JSON(..)
    , Query(..)
    ) where

import Blaze.ByteString.Builder (toByteString)
import Control.Arrow (first)
import Data.ByteString (ByteString)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString as B

-- | A placeholder for the SQL @NULL@ value.
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
--
-- The underlying type is a 'ByteString', and literal Haskell strings
-- that contain Unicode characters will be correctly transformed to
-- UTF-8.
newtype Query = Query {
      fromQuery :: ByteString
    } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . toByteString . Utf8.fromString

instance Semigroup Query where
    (<>) (Query a) (Query b) = Query (B.append a b)
    {-# INLINE (<>) #-}

instance Monoid Query where
    mempty = Query B.empty
    mappend = (<>)
    {-# INLINE mappend #-}

-- | A single-value \"collection\".
--
-- This is useful if you need to supply a single parameter to a SQL
-- query, or extract a single column from a SQL result.
--
-- Parameter example:
--
-- @query c \"select x from scores where x > ?\" ('Only' (42::Int))@
--
-- Result example:
--
-- @xs <- query_ c \"select id from users\"
--forM_ xs $ \\('Only' id) -> {- ... -}@
newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap a list of values for use in an @IN@ clause.  Replaces a
-- single \"@?@\" character with a parenthesized list of rendered
-- values.
--
-- Example:
--
-- > query c "select * from whatever where id in ?" (Only (In [3,4,5]))
newtype In a = In a
    deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap a list of values for use in a function with variable arguments.
-- Replaces a single \"@?@\" character with a non-parenthesized list of
-- rendered values.
--
-- Example:
--
-- > query conn
-- >   "SELECT * FROM example_table ORDER BY field(f,?)"
-- >   (Only (VaArgs [3,2,1]))
newtype VaArgs a = VaArgs a
  deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap a mostly-binary string to be escaped in hexadecimal.
newtype Binary a = Binary a
    deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap a JSON value.
newtype JSON a = JSON { fromJSON :: a }
    deriving (Eq, Ord, Read, Show, Typeable, Functor)
