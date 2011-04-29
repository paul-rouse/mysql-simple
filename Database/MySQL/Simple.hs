{-# LANGUAGE DeriveDataTypeable #-}

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
      FormatError(fmtMessage, fmtQuery, fmtParams)
    , Only(..)
    , Query
    , execute
    , execute_
    , query
    , query_
    , formatQuery
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Control.Applicative ((<$>), pure)
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, throw)
import Control.Monad.Fix (fix)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid (mappend)
import Data.Typeable (Typeable)
import Database.MySQL.Base (Connection)
import Database.MySQL.Simple.Param (Action(..), inQuotes)
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Database.MySQL.Simple.Types (Only(..), Query(..))
import qualified Data.ByteString.Char8 as B
import qualified Database.MySQL.Base as Base

data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

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

execute :: (QueryParams q) => Connection -> Query -> q -> IO Int64
execute conn template qs = do
  Base.query conn =<< formatQuery conn template qs
  finishExecute conn

execute_ :: Connection -> Query -> IO Int64
execute_ conn (Query stmt) = do
  Base.query conn stmt
  finishExecute conn

finishExecute :: Connection -> IO Int64
finishExecute conn = do
  ncols <- Base.fieldCount (Left conn)
  if ncols /= 0
    then error "execute: executed a select!"
    else Base.affectedRows conn
  
query :: (QueryParams q, QueryResults r) => Connection -> Query -> q -> IO [r]
query conn template qs = do
  Base.query conn =<< formatQuery conn template qs
  finishQuery conn
  
query_ :: (QueryResults r) => Connection -> Query -> IO [r]
query_ conn (Query q) = do
  Base.query conn q
  finishQuery conn

finishQuery :: (QueryResults r) => Connection -> IO [r]
finishQuery conn = do
  r <- Base.storeResult conn
  ncols <- Base.fieldCount (Right r)
  if ncols == 0
    then return []
    else do
      fs <- Base.fetchFields r
      flip fix [] $ \loop acc -> do
        row <- Base.fetchRow r
        case row of
          [] -> return (reverse acc)
          _  -> let c = convertResults fs row
                in rnf c `seq` loop (c:acc)

fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map twiddle xs
                    }
  where twiddle (Plain b)  = toByteString b
        twiddle (Escape s) = s
