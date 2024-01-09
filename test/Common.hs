{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}

module Common where

import Data.ByteString.Builder as BS
import Database.MySQL.Simple.Param

-- Declare some (orphan) instances needed for test specs
#if !MIN_VERSION_bytestring(0,11,1)
instance Show BS.Builder where
  show = show . BS.toLazyByteString
#endif

instance Eq BS.Builder where
  a == b = BS.toLazyByteString a == BS.toLazyByteString b

deriving instance Eq Action
