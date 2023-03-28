{-# LANGUAGE OverloadedStrings #-}
module CustomTypeSpec (
      customTypeUnit
    , customTypeSpec
) where

import Control.Monad                (void)
import qualified Data.ByteString.Char8 as B8
import Database.MySQL.Simple
import Database.MySQL.Simple.Param  (Action(..), render)
import qualified Database.MySQL.Base as MySQL
import Test.Hspec

import Common                       ()

-- A type which we encode/decode specially (if artificially!) to a Text column
--
data Message = English String | Latin String deriving (Eq, Show)

instance ToField Message where
    toField m = B8.pack ( case m of
                            English s -> "English: " <> s
                            Latin s   -> "Latin: " <> s
                        )
instance Param Message

instance FromField Message where
    fromField =
      ( [MySQL.TinyBlob, MySQL.Blob, MySQL.MediumBlob, MySQL.LongBlob]
      , \s -> if "English: " `B8.isPrefixOf` s then
                  Right $ English $ B8.unpack $ B8.drop 9 s
              else if "Latin: " `B8.isPrefixOf` s then
                  Right $ Latin $ B8.unpack $ B8.drop 7 s
              else Left $ "Can't decode " <> show s <> " as Message"
      )
instance Result Message


customTypeUnit :: Spec
customTypeUnit =
  describe "Param for custom type" $ do
    it "is correctly encoded" $ do
      render (Latin "nuntium") `shouldBe` Escape "Latin: nuntium"


customTypeSpec :: Connection -> Spec
customTypeSpec conn =
  beforeAll_
    ( do  _ <- execute_ conn "drop table if exists custom"
          _ <- execute_ conn "create table custom (i int, c text)"
          void $ execute_ conn ( "insert into custom (i,c) values "
                                 <> "(1,'English: a message'),"
                                 <> "(2,'French: un message')"
                               )
    ) $ do
      describe "reading a custom type" $ do
        it "should accept a correctly formatted field" $ do
          result <- query_ conn "select c from custom where i = 1"
          result `shouldBe` [Only (English "a message")]
        it "should reject an improperly formatted field" $ do
          (query_ conn "select c from custom where i = 2" :: IO [Only Message])
            `shouldThrow`
              (\e -> errMessage e == "Can't decode \"French: un message\" as Message")
      describe "writing a custom type" $ do
        it "should work with parameter substitution" $ do
          _ <- execute conn "insert into custom (i,c) values (?,?)"
                            (3::Int, Latin "nuntium")
          result <- query_ conn "select c from custom where i = 3"
          result `shouldBe` [Only (Latin "nuntium")]
