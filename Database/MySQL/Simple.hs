{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}

-- |
-- Module:      Database.MySQL.Simple
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- A mid-level client library for the MySQL database, aimed at ease of
-- use and high performance.

module Database.MySQL.Simple
    (
    -- * Types
      Query
    , Only(..)
    -- ** Exceptions
    , FormatError(fmtMessage, fmtQuery, fmtParams)
    , QueryError(qeMessage, qeQuery)
    , ResultError(errSQLType, errHaskellType, errMessage)
    -- * Connection management
    , Base.connect
    , Base.defaultConnectInfo
    , Base.close
    -- * Queries that return results
    , query
    , query_
    -- * Statements that do not return results
    , execute
    , execute_
    , Base.insertID
    -- * Transaction handling
    , withTransaction
    , Base.autocommit
    , Base.commit
    , Base.rollback
    -- * Helper functions
    , formatQuery
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Control.Applicative ((<$>), pure)
import Control.Exception (Exception, onException, throw)
import Control.Monad.Fix (fix)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid (mappend)
import Data.Typeable (Typeable)
import Database.MySQL.Base (Connection)
import Database.MySQL.Simple.Param (Action(..), inQuotes)
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.Result (ResultError(..))
import Database.MySQL.Simple.Types (Only(..), Query(..))
import qualified Data.ByteString.Char8 as B
import qualified Database.MySQL.Base as Base

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

-- | Exception thrown if 'query' is used to perform an @INSERT@-like
-- operation, or 'execute' is used to perform a @SELECT@-like operation.
data QueryError = QueryError {
      qeMessage :: String
    , qeQuery :: Query
    } deriving (Eq, Show, Typeable)

instance Exception QueryError

-- | Format a query string.
--
-- String parameters are escaped according to the character set in use
-- on the 'Connection'.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains a non-zero number of columns
--   (i.e. you should be using 'query' instead of 'execute').
formatQuery :: QueryParams q => Connection -> Query -> q -> IO ByteString
formatQuery conn q@(Query template) qs
    | null xs && '?' `B.notElem` template = return template
    | otherwise = toByteString . zipParams (split template) <$> mapM sub xs
  where xs = renderParams qs
        sub (Plain b)  = pure b
        sub (Escape s) = (inQuotes . fromByteString) <$> Base.escape conn s
        split s = fromByteString h : if B.null t then [] else split (B.tail t)
            where (h,t) = B.break (=='?') s
        zipParams (t:ts) (p:ps) = t `mappend` p `mappend` zipParams ts ps
        zipParams [t] []        = t
        zipParams _ _ = fmtError (show (B.count '?' template) ++
                                  " '?' characters, but " ++
                                  show (length xs) ++ " parameters") q xs

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the string could not be formatted correctly.
execute :: (QueryParams q) => Connection -> Query -> q -> IO Int64
execute conn template qs = do
  Base.query conn =<< formatQuery conn template qs
  finishExecute template conn

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO Int64
execute_ conn q@(Query stmt) = do
  Base.query conn stmt
  finishExecute q conn

finishExecute :: Query -> Connection -> IO Int64
finishExecute q conn = do
  ncols <- Base.fieldCount (Left conn)
  if ncols /= 0
    then throw $ QueryError ("execute resulted in " ++ show ncols ++
                             "-column result") q
    else Base.affectedRows conn

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results.
--
-- All results are retrieved and converted before this function
-- returns.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
query :: (QueryParams q, QueryResults r)
         => Connection -> Query -> q -> IO [r]
query conn template qs = do
  Base.query conn =<< formatQuery conn template qs
  finishQuery template conn

-- | A version of 'query' that does not perform query substitution.
query_ :: (QueryResults r) => Connection -> Query -> IO [r]
query_ conn q@(Query que) = do
  Base.query conn que
  finishQuery q conn

finishQuery :: (QueryResults r) => Query -> Connection -> IO [r]
finishQuery q conn = do
  r <- Base.storeResult conn
  ncols <- Base.fieldCount (Right r)
  if ncols == 0
    then throw $ QueryError "query resulted in zero-column result" q
    else do
      fs <- Base.fetchFields r
      flip fix [] $ \loop acc -> do
        row <- Base.fetchRow r
        case row of
          [] -> return (reverse acc)
          _  -> let !c = convertResults fs row
                in loop (c:acc)

-- | Execute an action inside a SQL transaction.
--
-- You are assumed to have started the transaction yourself.
--
-- If your action succeeds, the transaction will be 'Base.commit'ted
-- before this function returns.
--
-- If your action throws any exception (not just a SQL exception), the
-- transaction will be rolled back 'Base.rollback' before the
-- exception is propagated.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn act = do
  r <- act `onException` Base.rollback conn
  Base.commit conn
  return r

fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map twiddle xs
                    }
  where twiddle (Plain b)  = toByteString b
        twiddle (Escape s) = s
