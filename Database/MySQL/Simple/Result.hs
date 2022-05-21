{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings #-}
#if MIN_VERSION_time(1,5,0)
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

-- |
-- Module:      Database.MySQL.Simple.Result
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- The 'Result' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- MySQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the MySQL 'Long' type
-- because it can represent a 'Long' exactly. On the other hand, since
-- a 'Double' might lose precision if representing a 'LongLong', the
-- two are /not/ considered compatible.

module Database.MySQL.Simple.Result
    (
      Result(..)
    , ResultError(..)
    ) where

#include "MachDeps.h"

import Control.Applicative ((<$>), (<*>), (<*), pure)
import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (TimeOfDay, makeTimeOfDayValid)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Database.MySQL.Base.Types (Field(..), Type(..))
import Database.MySQL.Simple.Types (JSON(..))
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: String
                                , errHaskellType :: String
                                , errFieldName :: String
                                , errMessage :: String }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: String
                                  , errHaskellType :: String
                                  , errFieldName :: String
                                  , errMessage :: String }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: String
                                    , errHaskellType :: String
                                    , errFieldName :: String
                                    , errMessage :: String }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

-- | A type that may be converted from a SQL type.
class Result a where
    convert :: Field -> Maybe ByteString -> a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Throws a 'ResultError' if conversion fails.

instance (Result a) => Result (Maybe a) where
    convert _ Nothing = Nothing
    convert f bs      = Just (convert f bs)

instance Result Bool where
    convert = atto ok8 ((/=(0::Int)) <$> decimal)

instance Result Int8 where
    convert = atto ok8 $ signed decimal

instance Result Int16 where
    convert = atto ok16 $ signed decimal

instance Result Int32 where
    convert = atto ok32 $ signed decimal

instance Result Int where
    convert = atto okWord $ signed decimal

instance Result Int64 where
    convert = atto ok64 $ signed decimal

instance Result Integer where
    convert = atto ok64 $ signed decimal

instance Result Word8 where
    convert = atto ok8 decimal

instance Result Word16 where
    convert = atto ok16 decimal

instance Result Word32 where
    convert = atto ok32 decimal

instance Result Word where
    convert = atto okWord decimal

instance Result Word64 where
    convert = atto ok64 decimal

instance Result Float where
    convert = atto ok (realToFrac <$> double)
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24]

instance Result Double where
    convert = atto ok double
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,
                              Long]

instance Result (Ratio Integer) where
    convert = atto ok rational
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,
                              Long,LongLong]

instance Result SB.ByteString where
    convert f = doConvert f okText $ id

instance Result LB.ByteString where
    convert f = LB.fromChunks . (:[]) . convert f

instance Result ST.Text where
    convert f | isText f  = doConvert f okText $ ST.decodeUtf8
              | otherwise = incompatible f (typeOf ST.empty)
                            "attempt to mix binary and text"

instance Result LT.Text where
    convert f = LT.fromStrict . convert f

instance Result [Char] where
    convert f = ST.unpack . convert f

instance Result UTCTime where
    convert f = doConvert f ok $ \bs ->
                case parseTimeM True defaultTimeLocale "%F %T%Q" (B8.unpack bs) of
                  Just t -> t
                  Nothing
                    | SB.isPrefixOf "0000-00-00" bs ->
                        UTCTime (fromGregorian 0 0 0) 0
                    | otherwise -> conversionFailed f "UTCTime" "could not parse"
        where ok = mkCompats [DateTime,Timestamp]

instance Result Day where
    convert f = flip (atto ok) f $ case fieldType f of
                                     Year -> year
                                     _    -> date
        where ok = mkCompats [Year,Date,NewDate]
              year = fromGregorian <$> decimal <*> pure 1 <*> pure 1
              date = fromGregorian <$> (decimal <* char '-')
                                   <*> (decimal <* char '-')
                                   <*> decimal

instance Result TimeOfDay where
    convert f = doConvert f ok $ \bs ->
                case parseTimeM True defaultTimeLocale "%T%Q" (B8.unpack bs) of
                  Just t -> t
                  _      -> conversionFailed f "TimeOfDay" "could not parse"
        where ok = mkCompats [Time]

instance (Typeable a, FromJSON a) => Result (JSON a) where
    convert f = doConvert f ok $ \bs ->
                case Aeson.eitherDecodeStrict' bs of
                  Right a -> JSON a
                  Left err -> conversionFailed f "JSON" err
        where ok = mkCompats [Json]

isText :: Field -> Bool
isText f = fieldCharSet f /= 63

newtype Compat = Compat Word32

mkCompats :: [Type] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: Type -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0

okText, ok8, ok16, ok32, ok64, okWord :: Compat
okText = mkCompats [VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,
                    Set,Enum,Json]
ok8 = mkCompats [Tiny]
ok16 = mkCompats [Tiny,Short]
ok32 = mkCompats [Tiny,Short,Int24,Long]
ok64 = mkCompats [Tiny,Short,Int24,Long,LongLong]
#if WORD_SIZE_IN_BITS < 64
okWord = ok32
#else
okWord = ok64
#endif

doConvert :: (Typeable a) =>
             Field -> Compat -> (ByteString -> a) -> Maybe ByteString -> a
doConvert f types cvt (Just bs)
    | mkCompat (fieldType f) `compat` types = cvt bs
    | otherwise = incompatible f (typeOf (cvt undefined)) "types incompatible"
doConvert f _ cvt _ = throw $ UnexpectedNull (show (fieldType f))
                              (show (typeOf (cvt undefined)))
                              (B8.unpack (fieldName f))
                              ("unexpected null in table "
                               ++ B8.unpack (fieldTable f)
                               ++ " of database "
                               ++ B8.unpack (fieldDB f)
                              )

incompatible :: Field -> TypeRep -> String -> a
incompatible f r = throw . Incompatible (show (fieldType f))
                                        (show r)
                                        (B8.unpack (fieldName f))

conversionFailed :: Field -> String -> String -> a
conversionFailed f s = throw . ConversionFailed (show (fieldType f)) s
                                 (B8.unpack (fieldName f))

atto :: (Typeable a) => Compat -> Parser a -> Field -> Maybe ByteString -> a
atto types p0 f = doConvert f types $ go undefined p0
  where
    go :: (Typeable a) => a -> Parser a -> ByteString -> a
    go dummy p s =
        case parseOnly p s of
          Left err -> conversionFailed f (show (typeOf dummy)) err
          Right v  -> v
