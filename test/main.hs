{-# LANGUAGE CPP, OverloadedStrings #-}

{-# options_ghc -fno-warn-orphans #-}

import Data.ByteString.Builder as BS
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

  describe "splitQuery" $ do
    it "works for a single question mark" $ do
      splitQuery "select * from foo where name = ?"
        `shouldBe`
          ["select * from foo where name = ", ""]
    it "works with a question mark in a string literal" $ do
      splitQuery "select 'hello?'"
        `shouldBe`
          ["select 'hello?'"]
    it "works with many question marks" $ do
      splitQuery "select ? + ? + what from foo where bar = ?"
        `shouldBe`
          ["select ", " + ", " + what from foo where bar = ", ""]

instance Show BS.Builder where
  show = show . BS.toLazyByteString

instance Eq BS.Builder where
  a == b = BS.toLazyByteString a == BS.toLazyByteString b

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
      result <- query conn "select 'hello?'" ()
      result `shouldBe` [Only ("hello?" :: Text)]
    describe "with too many query params" $ do
      it "should have the right message" $ do
        (query conn "select 'hello?'" (Only ['a']) :: IO [Only Text])
          `shouldThrow`
            (\e -> fmtMessage e == "0 '?' characters, but 1 parameters")
    describe "with too few query params" $ do
      it "should have the right message" $ do
        (query conn "select 'hello?' = ?" () :: IO [Only Text])
          `shouldThrow`
            (\e -> fmtMessage e == "1 '?' characters, but 0 parameters")
  describe "formatQuery" $ do
    it "should not blow up on a question mark in string literal" $ do
      formatQuery conn "select 'hello?'" ()
        `shouldReturn`
          "select 'hello?'"
