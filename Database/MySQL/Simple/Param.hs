{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Database.MySQL.Simple.Param
    (
      Action(..)
    , Param(..)
    , inQuotes
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import Blaze.Text (integral, double, float)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mappend)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Database.MySQL.Simple.Types (Null)
import System.Locale (defaultTimeLocale)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

data Action = Plain Builder
            | Escape ByteString

class Param a where
    render :: a -> Action

instance Param Action where
    render a = a
    {-# INLINE render #-}

instance (Param a) => Param (Maybe a) where
    render Nothing  = renderNull
    render (Just a) = render a
    {-# INLINE render #-}

renderNull :: Action
renderNull = Plain (fromByteString "null")

instance Param Null where
    render _ = renderNull
    {-# INLINE render #-}

instance Param Bool where
    render = Plain . integral . fromEnum
    {-# INLINE render #-}

instance Param Int8 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int16 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int32 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int64 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Integer where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word8 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word16 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word32 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word64 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Float where
    render v | isNaN v || isInfinite v = renderNull
             | otherwise               = Plain (float v)
    {-# INLINE render #-}

instance Param Double where
    render v | isNaN v || isInfinite v = renderNull
             | otherwise               = Plain (double v)
    {-# INLINE render #-}

instance Param SB.ByteString where
    render = Escape
    {-# INLINE render #-}

instance Param LB.ByteString where
    render = render . SB.concat . LB.toChunks
    {-# INLINE render #-}

instance Param ST.Text where
    render = Escape . ST.encodeUtf8
    {-# INLINE render #-}

instance Param [Char] where
    render = Escape . toByteString . Utf8.fromString
    {-# INLINE render #-}

instance Param LT.Text where
    render = render . LT.toStrict
    {-# INLINE render #-}

instance Param UTCTime where
    render = Plain . Utf8.fromString . formatTime defaultTimeLocale "'%F %T'"
    {-# INLINE render #-}

instance Param Day where
    render = Plain . inQuotes . Utf8.fromString . showGregorian
    {-# INLINE render #-}

instance Param TimeOfDay where
    render = Plain . inQuotes . Utf8.fromString . show
    {-# INLINE render #-}

inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = Utf8.fromChar '\''
