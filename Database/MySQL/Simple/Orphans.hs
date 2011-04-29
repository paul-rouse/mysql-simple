{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Database.MySQL.Orphans
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Orphan instances of frequently used typeclasses for types that
-- really should have them.

module Database.MySQL.Simple.Orphans () where

import Control.DeepSeq (NFData(..))
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime (TimeOfDay(..))
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy.Internal as LB

instance NFData SB.ByteString where
    rnf (SB.PS _ _ _) = ()
    {-# INLINE rnf #-}

instance NFData LB.ByteString where
    rnf (LB.Chunk (SB.PS _ _ _) cs) = rnf cs
    rnf LB.Empty = ()

instance NFData Day where
    rnf (ModifiedJulianDay !_) = ()
    {-# INLINE rnf #-}

instance NFData TimeOfDay where
    rnf (TimeOfDay !_ !_ !_) = ()
    {-# INLINE rnf #-}

instance NFData UTCTime where
    rnf (UTCTime !_ !_) = ()
    {-# INLINE rnf #-}

instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f,
          NFData g, NFData h, NFData i, NFData j) =>
    NFData (a,b,c,d,e,f,g,h,i,j)
  where
    rnf (a,b,c,d,e,f,g,h,i,j) =
      rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
      rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j
    {-# INLINE rnf #-}
