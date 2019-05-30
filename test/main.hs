{-# LANGUAGE CPP, OverloadedStrings, StandaloneDeriving,
  DerivingStrategies, DeriveGeneric, DeriveAnyClass, RankNTypes #-}

import Data.Text (Text)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Base.Types (Field(Field), Type(Tiny))
import Test.Hspec
import qualified Data.ByteString.Char8 as ByteString
import Blaze.ByteString.Builder (toByteString)
#if MIN_VERSION_base(4,8,2)
#else
import Control.Applicative
import Data.Monoid
#endif
import GHC.Generics
import Database.MySQL.Simple.QueryResults
import Data.Int (Int8)
import Control.Monad (void)

-- This is how to connect to our test database
testConn :: ConnectInfo
testConn = defaultConnectInfo {
               connectHost     = "127.0.0.1",
               connectUser     = "test",
               connectDatabase = "test"
           }

main :: IO ()
main =
  hspec $ do
    unitSpec
    genericQueryResultsSpec
    bracket' runIO (connect testConn) close integrationSpec

-- | Ad-hoc lifted version of 'bracket'.  The typical 'liftIO' method
-- is a parameter to this function rather than a constraint since
-- 'SpecM' from "Test.Hspec.Core.Spec" is not an instance of
-- 'MonadIO'.
bracket'
  :: Monad io
  => (forall x . IO x -> io x)
  -> IO a
  -> (a -> IO b)
  -> (a -> io b)
  -> io b
bracket' liftIO' acq rel act = do
  conn <- liftIO' acq
  a <- act conn
  void $ liftIO' $ rel conn
  pure a

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

data U = U Int8 Int8 Int8

deriving stock    instance Eq U
deriving stock    instance Generic U
deriving stock    instance Show U
deriving anyclass instance QueryResults U

data T = T Int8 Int8 Int8 Int8 Int8 Int8

deriving stock    instance Eq T
deriving stock    instance Generic T
deriving stock    instance Show T
deriving anyclass instance QueryResults T

genericQueryResultsSpec :: Spec
genericQueryResultsSpec =
  describe "QueryResults" $
    it "can perform generic conversions" $ do
      convert [0..2] `shouldBe` U 0 1 2
      convert [0..5] `shouldBe` T 0 1 2 3 4 5
  where
  aField :: Field
  aField = Field mempty mempty mempty mempty
    mempty mempty mempty 0 0 mempty 0 0 Tiny
  convert :: QueryResults a => [Int8] -> a
  convert xs = convertResults
    (replicate (length xs) aField) $ Just . ByteString.pack . show <$> xs
