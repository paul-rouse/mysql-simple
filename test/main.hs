{-# LANGUAGE CPP, OverloadedStrings #-}

import Control.Applicative   ((<|>))
import Control.Exception (bracket)
import Data.Text (Text)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import System.Environment    (getEnvironment)
import Test.Hspec
import Blaze.ByteString.Builder (toByteString)

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

main :: IO ()
main = do
    ci <- isCI
    bracket (connect $ testConn ci) close $ \conn ->
      hspec $ do
        unitSpec
        integrationSpec conn

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
  describe "the library" $ do
    it "can connect to a database" $ do
      result <- query_ conn "select 1 + 1"
      result `shouldBe` [Only (2::Int)]
