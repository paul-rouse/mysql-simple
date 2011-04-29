module Database.MySQL.Simple.QueryResults
    (
      QueryResults(..)
    ) where

import Data.ByteString (ByteString)
import Database.MySQL.Base.Types
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types

class QueryResults a where
    convertResults :: [Field] -> [Maybe ByteString] -> a

instance (Result a) => QueryResults (Only a) where
    convertResults [fa] [va] = Only (convert fa va)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b) => QueryResults (a,b) where
    convertResults [fa,fb] [va,vb] = (convert fa va, convert fb vb)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c) => QueryResults (a,b,c) where
    convertResults [fa,fb,fc] [va,vb,vc] =
        (convert fa va, convert fb vb, convert fc vc)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d) =>
    QueryResults (a,b,c,d) where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e) =>
    QueryResults (a,b,c,d,e) where
    convertResults [fa,fb,fc,fd,fe] [va,vb,vc,vd,ve] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e, Result f) =>
    QueryResults (a,b,c,d,e,f) where
    convertResults [fa,fb,fc,fd,fe,ff] [va,vb,vc,vd,ve,vf] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve, convert ff vf)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g) =>
    QueryResults (a,b,c,d,e,f,g) where
    convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve, convert ff vf, convert fg vg)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h) =>
    QueryResults (a,b,c,d,e,f,g,h) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh] [va,vb,vc,vd,ve,vf,vg,vh] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve, convert ff vf, convert fg vg, convert fh vh)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i) =>
    QueryResults (a,b,c,d,e,f,g,h,i) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi] [va,vb,vc,vd,ve,vf,vg,vh,vi] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve, convert ff vf, convert fg vg, convert fh vh,
         convert fi vi)
    convertResults fs vs  = convError fs vs

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj] =
        (convert fa va, convert fb vb, convert fc vc, convert fd vd,
         convert fe ve, convert ff vf, convert fg vg, convert fh vh,
         convert fi vi, convert fj vj)
    convertResults fs vs  = convError fs vs

convError :: [Field] -> [Maybe ByteString] -> a
convError = error "convError"
