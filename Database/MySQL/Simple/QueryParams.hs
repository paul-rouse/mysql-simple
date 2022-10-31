-- |
-- Module:      Database.MySQL.Simple.QueryParams
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- The 'QueryParams' typeclass, for rendering a collection of
-- parameters to a SQL query.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.

module Database.MySQL.Simple.QueryParams
    (
      QueryParams(..)
    ) where

import Database.MySQL.Simple.Param (Action(..), Param(..))
import Database.MySQL.Simple.Types (Only(..))

-- | A collection type that can be turned into a list of rendering
-- 'Action's.
--
-- Instances should use the 'render' method of the 'Param' class
-- to perform conversion of each element of the collection.
class QueryParams a where
    renderParams :: a -> [Action]
    -- ^ Render a collection of values.

instance QueryParams () where
    renderParams _ = []

instance (Param a) => QueryParams (Only a) where
    renderParams (Only v) = [render v]

instance (Param a, Param b) => QueryParams (a,b) where
    renderParams (a,b) = [render a, render b]

instance (Param a, Param b, Param c) => QueryParams (a,b,c) where
    renderParams (a,b,c) = [render a, render b, render c]

instance (Param a, Param b, Param c, Param d) => QueryParams (a,b,c,d) where
    renderParams (a,b,c,d) = [render a, render b, render c, render d]

instance (Param a, Param b, Param c, Param d, Param e)
    => QueryParams (a,b,c,d,e) where
    renderParams (a,b,c,d,e) =
        [render a, render b, render c, render d, render e]

instance (Param a, Param b, Param c, Param d, Param e, Param f)
    => QueryParams (a,b,c,d,e,f) where
    renderParams (a,b,c,d,e,f) =
        [render a, render b, render c, render d, render e, render f]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g)
    => QueryParams (a,b,c,d,e,f,g) where
    renderParams (a,b,c,d,e,f,g) =
        [render a, render b, render c, render d, render e, render f, render g]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h)
    => QueryParams (a,b,c,d,e,f,g,h) where
    renderParams (a,b,c,d,e,f,g,h) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i)
    => QueryParams (a,b,c,d,e,f,g,h,i) where
    renderParams (a,b,c,d,e,f,g,h,i) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j)
    => QueryParams (a,b,c,d,e,f,g,h,i,j) where
    renderParams (a,b,c,d,e,f,g,h,i,j) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s, Param t)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s, render t]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s, Param t, Param u)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s, render t, render u]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s, Param t, Param u,
          Param v)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s, render t, render u,
         render v]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s, Param t, Param u,
          Param v, Param w)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s, render t, render u,
         render v, render w]

instance (Param a, Param b, Param c, Param d, Param e, Param f, Param g,
          Param h, Param i, Param j, Param k, Param l, Param m, Param n,
          Param o, Param p, Param q, Param r, Param s, Param t, Param u,
          Param v, Param w, Param x)
    => QueryParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
    renderParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) =
        [render a, render b, render c, render d, render e, render f, render g,
         render h, render i, render j, render k, render l, render m, render n,
         render o, render p, render q, render r, render s, render t, render u,
         render v, render w, render x]

instance (Param a) => QueryParams [a] where
    renderParams = map render
