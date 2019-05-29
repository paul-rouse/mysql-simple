{-| A quasi-quoter for SQL expressions. -}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Database.MySQL.Simple.QQ
  ( sql
  ) where

import Prelude ()
import Database.MySQL.Simple.Prelude
import Language.Haskell.TH          (Exp, Q, appE, stringE)
import Language.Haskell.TH.Quote    (QuasiQuoter (..))
import Database.MySQL.Simple        (Query, Query)
import Text.Printf                  (printf)

-- | A quasi-quoter for SQL expressions.
--
-- The quasi-quoter does not do any sort of parsing of the SQL.  It's
-- simply a convenience for writing multi-line SQL statements.  So in stead of:
--
-- > query
-- >   =  "select * "
-- >   <> "from users "
-- >   <> "where email is null;"
--
-- You could write
--
-- > query = [sql|
-- >   select *
-- >   from users
-- >   where email is null;
-- > |]
--
-- Note the quasi-quoter is only valid in expression contexts.
--
-- @since 0.4.7
sql :: QuasiQuoter
sql = QuasiQuoter
  { quotePat  = err "pattern"
  , quoteType = error "Database.MySQL.Simple.QQ.sql: quasiquoter used in type context"
  , quoteDec  = error "Database.MySQL.Simple.QQ.sql: quasiquoter used in declaration context"
  , quoteExp  = quote
  }
  where
  err :: String -> a
  err ctxt = error (printf "Database.MySQL.Simple.QQ.sql: quasiquoter used in %s context" ctxt)

quote :: String -> Q Exp
quote = appE [| fromString :: String -> Query |] . stringE
