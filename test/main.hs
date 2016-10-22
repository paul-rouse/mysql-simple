{-# LANGUAGE OverloadedStrings          #-}

import Control.Exception     (bracket)
import Database.MySQL.Simple (ConnectInfo (..), defaultConnectInfo,
                              connect, close,
                              query_, Only (..))
import Test.Hspec

-- This is how to connect to our test database
testConn :: ConnectInfo
testConn = defaultConnectInfo {
               connectHost     = "127.0.0.1",
               connectUser     = "test",
               connectDatabase = "test"
           }

-- Only the most cursory test is done at the moment, simply showing that
-- things hang together sufficiently well that we can talk to the database
-- server.
--
main :: IO ()
main = bracket (connect testConn) close $ \conn -> hspec $ do
    describe "Database" $ do
      it "seems to be connected" $ do
        result <- query_ conn "select 1 + 1"
        result `shouldBe` [Only (2::Int)]
