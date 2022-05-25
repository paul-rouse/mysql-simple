{-# LANGUAGE CPP, OverloadedStrings #-}

import Control.Applicative   ((<|>))
import Control.Exception (bracket)
import Data.Text (Text)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import System.Environment    (getEnvironment)
import Test.Hspec
import Blaze.ByteString.Builder (toByteString)

import Common                       ()
import DateTimeSpec                 (dateTimeUnit, dateTimeSpec)

isCI :: IO Bool
isCI = do
    env <- getEnvironment
    return $ case lookup "TRAVIS" env <|> lookup "CI" env of
               Just "true" -> True
               _ -> False

-- This is how to connect to our test database
testConn :: Bool -> ConnectInfo
testConn ci = defaultConnectInfo {
                connectHost     = "127.0.0.1"
              , connectUser     = "test"
              , connectPassword = "test"
              , connectDatabase = "test"
              , connectPort     = if ci then 33306 else 3306
              }

-- Allow tests to do things which would be prevented in strict SQL mode
withConnection :: (Connection -> IO()) -> IO()
withConnection action = do
    ci <- isCI
    bracket (do  conn <- connect $ testConn ci
                 _ <- execute_ conn "set session sql_mode=''"
                 return conn
            )
            close
            action

main :: IO ()
main =
    withConnection $ \conn ->
      hspec $ do
        describe "Database.MySQL.Simple.unitSpec" unitSpec
        describe "Database.MySQL.Simple.integrationSpec" $ integrationSpec conn
        describe "Database.MySQL.Simple.DateTimeSpec" $ do
          dateTimeUnit
          dateTimeSpec conn

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
