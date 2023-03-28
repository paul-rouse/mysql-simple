{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures #-}
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
    ( FromField(..)
    , Result(..)
    , ResultError(..)
    ) where

#include "MachDeps.h"

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Control.Applicative ((<$>), (<*>), (<*), pure)
import Control.Exception (Exception, throw)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (parseTimeM, ParseTime)
import Data.Time.LocalTime (TimeOfDay, makeTimeOfDayValid)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Database.MySQL.Base.Types (Field(..), Type(..))
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

-- | A type that can be converted from a 'ByteString'.  Any type which is
-- an instance of this class, and is 'Typeable', can use the default
-- implementation of 'Result'.  This provides a method of implementing
-- a decoder for any text-like column, such as @TEXT@, @BLOB@, or @JSON@,
-- instead of implementing 'Result' directly.
--
-- The first component of the tuple returned by 'fromField' is a list of
-- acceptable column types, expressed in terms of
-- 'Database.MySQL.Base.Types.Type'.
--
-- @since 0.4.8
--
class FromField a where
    fromField :: ([Type], ByteString -> Either String a)

-- | A type that may be converted from a SQL type.
--
-- A default implementation is provided for any type which is an instance
-- of both 'FromField' and 'Typeable', providing a simple mechanism for
-- user-defined decoding from text- or blob-like fields (including @JSON@).
--
class Result a where
    convert :: Field -> Maybe ByteString -> a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Throws a 'ResultError' if conversion fails.
    --
    default convert :: (Typeable a, FromField a)
                       => Field -> Maybe ByteString -> a
    convert f =
        doConvert f (mkCompats allowTypes) $ \bs ->
            case cvt bs of
              Right x  -> x
              Left err -> conversionFailed f (show (typeOf (cvt undefined))) err
        where
            (allowTypes, cvt) = fromField


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
    convert f = doConvert f okText id

instance Result LB.ByteString where
    convert f = LB.fromChunks . (:[]) . convert f

instance Result ST.Text where
    convert f | isText f  = doConvert f okText ST.decodeUtf8
              | otherwise = incompatible f (typeOf ST.empty)
                            "attempt to mix binary and text"

instance Result LT.Text where
    convert f = LT.fromStrict . convert f

instance Result [Char] where
    convert f = ST.unpack . convert f

instance FromField UTCTime where
    fromField =
        ( [DateTime, Timestamp]
        , \bs -> if "0000-00-00" `SB.isPrefixOf` bs then
                     -- https://dev.mysql.com/doc/refman/8.0/en/datetime.html
                     Right $ UTCTime (fromGregorian 0 0 0) 0
                 else
                     parseTimeField "%F %T%Q" (B8.unpack bs)
        )
instance Result UTCTime

instance Result Day where
    convert f = flip (atto ok) f $ case fieldType f of
                                     Year -> year
                                     _    -> date
        where ok = mkCompats [Year,Date,NewDate]
              year = fromGregorian <$> decimal <*> pure 1 <*> pure 1
              date = fromGregorian <$> (decimal <* char '-')
                                   <*> (decimal <* char '-')
                                   <*> decimal

instance FromField TimeOfDay where
    fromField = ([Time], parseTimeField "%T%Q" . B8.unpack)
instance Result TimeOfDay

-- A specialised version of parseTimeM which builds in the defaults we want,
-- and produces an Either result.  For the latter provide a wrapper for Either,
-- local to this module, to add a MonadFail instance.
--
newtype Failable a = Failable { failable :: Either String a }
                     deriving (Functor, Applicative, Monad)
instance MonadFail Failable where
    fail err = Failable (Left err)

parseTimeField :: ParseTime t => String -> String -> Either String t
parseTimeField fmt s = failable $ parseTimeM True defaultTimeLocale fmt s

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
