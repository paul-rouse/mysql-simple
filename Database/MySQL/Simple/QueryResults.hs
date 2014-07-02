{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Database.MySQL.Simpe.QueryResults
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'QueryResults' typeclass, for converting a row of results
-- returned by a SQL query into a more useful Haskell representation.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.

module Database.MySQL.Simple.QueryResults
    (
      QueryResults(..)
    , convertError
    ) where

import Control.Exception (throw)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Database.MySQL.Base.Types (Field(fieldType))
import Database.MySQL.Simple.Result (ResultError(..), Result(..))
import Database.MySQL.Simple.Types (Only(..))

-- | A collection type that can be converted from a list of strings.
--
-- Instances should use the 'convert' method of the 'Result' class
-- to perform conversion of each element of the collection.
--
-- This example instance demonstrates how to convert a two-column row
-- into a Haskell pair. Each field in the metadata is paired up with
-- each value from the row, and the two are passed to 'convert'.
--
-- @
-- instance ('Result' a, 'Result' b) => 'QueryResults' (a,b) where
--     'convertResults' [fa,fb] [va,vb] = (a,b)
--         where !a = 'convert' fa va
--               !b = 'convert' fb vb
--     'convertResults' fs vs  = 'convertError' fs vs 2
-- @
--
-- Notice that this instance evaluates each element to WHNF before
-- constructing the pair. By doing this, we guarantee two important
-- properties:
--
-- * Keep resource usage under control by preventing the construction
--   of potentially long-lived thunks.
--
-- * Ensure that any 'ResultError' that might arise is thrown
--   immediately, rather than some place later in application code
--   that cannot handle it.
--
-- You can also declare Haskell types of your own to be instances of
-- 'QueryResults'.
--
-- @
--data User = User { firstName :: String, lastName :: String }
--
--instance 'QueryResults' User where
--    'convertResults' [fa,fb] [va,vb] = User <$> a <*> b
--        where !a = 'convert' fa va
--              !b = 'convert' fb vb
--    'convertResults' fs vs  = 'convertError' fs vs 2
-- @

class QueryResults a where
    convertResults :: [Field] -> [Maybe ByteString] -> a
    -- ^ Convert values from a row into a Haskell collection.
    --
    -- This function will throw a 'ResultError' if conversion of the
    -- collection fails.

instance (Result a) => QueryResults (Only a) where
    convertResults [fa] [va] = Only a
        where !a = convert fa va
    convertResults fs vs  = convertError fs vs 1

instance (Result a, Result b) => QueryResults (a,b) where
    convertResults [fa,fb] [va,vb] = (a,b)
        where !a = convert fa va; !b = convert fb vb
    convertResults fs vs  = convertError fs vs 2

instance (Result a, Result b, Result c) => QueryResults (a,b,c) where
    convertResults [fa,fb,fc] [va,vb,vc] = (a,b,c)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
    convertResults fs vs  = convertError fs vs 3

instance (Result a, Result b, Result c, Result d) =>
    QueryResults (a,b,c,d) where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = (a,b,c,d)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd
    convertResults fs vs  = convertError fs vs 4

instance (Result a, Result b, Result c, Result d, Result e) =>
    QueryResults (a,b,c,d,e) where
    convertResults [fa,fb,fc,fd,fe] [va,vb,vc,vd,ve] = (a,b,c,d,e)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve
    convertResults fs vs  = convertError fs vs 5

instance (Result a, Result b, Result c, Result d, Result e, Result f) =>
    QueryResults (a,b,c,d,e,f) where
    convertResults [fa,fb,fc,fd,fe,ff] [va,vb,vc,vd,ve,vf] = (a,b,c,d,e,f)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
    convertResults fs vs  = convertError fs vs 6

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g) =>
    QueryResults (a,b,c,d,e,f,g) where
    convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] =
        (a,b,c,d,e,f,g)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg
    convertResults fs vs  = convertError fs vs 7

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h) =>
    QueryResults (a,b,c,d,e,f,g,h) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh] [va,vb,vc,vd,ve,vf,vg,vh] =
        (a,b,c,d,e,f,g,h)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh
    convertResults fs vs  = convertError fs vs 8

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i) =>
    QueryResults (a,b,c,d,e,f,g,h,i) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi] [va,vb,vc,vd,ve,vf,vg,vh,vi] =
        (a,b,c,d,e,f,g,h,i)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
    convertResults fs vs  = convertError fs vs 9

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj] =
        (a,b,c,d,e,f,g,h,i,j)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj
    convertResults fs vs  = convertError fs vs 10

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j, Result k) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j,k) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk] =
        (a,b,c,d,e,f,g,h,i,j,k)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj; !k = convert fk vk
    convertResults fs vs  = convertError fs vs 11

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j, Result k, Result l) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j,k,l) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl] =
        (a,b,c,d,e,f,g,h,i,j,k,l)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj; !k = convert fk vk; !l = convert fl vl
    convertResults fs vs  = convertError fs vs 12

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j, Result k, Result l,
          Result m) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl,fm]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm] =
        (a,b,c,d,e,f,g,h,i,j,k,l,m)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj; !k = convert fk vk; !l = convert fl vl
              !m = convert fm vm
    convertResults fs vs  = convertError fs vs 13

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j, Result k, Result l,
          Result m, Result n) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl,fm,fn]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn] =
        (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj; !k = convert fk vk; !l = convert fl vl
              !m = convert fm vm; !n = convert fn vn
    convertResults fs vs  = convertError fs vs 14

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j, Result k, Result l,
          Result m, Result n, Result o) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl,fm,fn,fo]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo] =
        (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj; !k = convert fk vk; !l = convert fl vl
              !m = convert fm vm; !n = convert fn vn; !o = convert fo vo
    convertResults fs vs  = convertError fs vs 15

-- | Throw a 'ConversionFailed' exception, indicating a mismatch
-- between the number of columns in the 'Field' and row, and the
-- number in the collection to be converted to.
convertError :: [Field]
             -- ^ Descriptors of fields to be converted.
             -> [Maybe ByteString]
             -- ^ Contents of the row to be converted.
             -> Int
             -- ^ Number of columns expected for conversion.  For
             -- instance, if converting to a 3-tuple, the number to
             -- provide here would be 3.
             -> a
convertError fs vs n = throw $ ConversionFailed
    (show (length fs) ++ " values: " ++ show (zip (map fieldType fs)
                                                  (map (fmap ellipsis) vs)))
    (show n ++ " slots in target type")
    "mismatch between number of columns to convert and number in target type"

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs
