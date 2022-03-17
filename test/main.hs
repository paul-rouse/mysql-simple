{-# LANGUAGE CPP, OverloadedStrings #-}

import Control.Exception (bracket)
import Data.Text (Text)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Test.Hspec
import Blaze.ByteString.Builder (toByteString)
#if MIN_VERSION_base(4,8,2)
#else
import Control.Applicative
import Data.Monoid
#endif

-- This is how to connect to our test database
testConn :: ConnectInfo
testConn = defaultConnectInfo {
               connectHost     = "127.0.0.1",
               connectUser     = "test",
               connectDatabase = "test"
           }

main :: IO ()
main =
  bracket (connect testConn) close $ \conn ->
    hspec $ do
      describe "Database.MySQL.Simple.unitSpec" unitSpec
      describe "Database.MySQL.Simple.integrationSpec" $ integrationSpec conn

unitSpec :: Spec
unitSpec = do
  describe "Param a => Param (In [a]) instance" $ do
    it "renders an empty list correctly" $ do
      let empty :: [Text]
          empty = mempty

      case render (In empty) of
        Plain a -> toByteString a `shouldBe` "(null)"
        _       -> expectationFailure "expected a Plain"

    it "renders a non-empty list correctly" $ do
      let l :: [Text]
          l = ["foo", "bar"]

      case render (In l) of
        Many [Plain _, Escape "foo", Plain _, Escape "bar", Plain _] -> pure ()
        _ -> expectationFailure "expected a Many with specific contents"

integrationSpec :: Connection -> Spec
integrationSpec conn = do
  describe "query_" $ do
    it "can connect to a database" $ do
      result <- query_ conn "select 1 + 1"
      result `shouldBe` [Only (2::Int)]
    it "can have question marks in string literals" $ do
      result <- query_ conn "select 'hello?'"
      result `shouldBe` [Only ("hello?" :: Text)]
  describe "query" $ do
    it "can have question marks in string literals" $ do
      result <- query conn "select 'hello?'" ([] :: [Int])
      result `shouldBe` [Only ("hello?" :: Text)]
