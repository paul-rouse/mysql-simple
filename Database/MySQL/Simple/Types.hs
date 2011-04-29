{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.MySQL.Simple.Types
    (
      Null(..)
    , Only(..)
    , Query(..)
    ) where

import Control.Arrow
import Control.DeepSeq (NFData)
import Blaze.ByteString.Builder
import Data.String (IsString(..))
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import Data.ByteString (ByteString)

data Null = Null

newtype Query = Query {
      fromQuery :: ByteString
    } deriving (Eq, Ord)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . toByteString . Utf8.fromString

newtype Only a = Only a
    deriving (Eq, Ord, Read, Show, NFData)
