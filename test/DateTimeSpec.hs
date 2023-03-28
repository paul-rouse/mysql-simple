{-# LANGUAGE OverloadedStrings #-}
module DateTimeSpec (
      dateTimeUnit
    , dateTimeSpec
) where

import Control.Monad                (void)
import Data.Ratio                   ((%))
import Data.Time.Calendar           (Day(..), toGregorian, fromGregorian)
import Data.Time.Clock              (UTCTime(..))
import Data.Time.LocalTime          (TimeOfDay(..))
import Database.MySQL.Simple
import Database.MySQL.Simple.Param  (Action(..), render)
import Test.Hspec

import Common                       ()

-- An arbitrary date and time: 2022-05-25 13:09:34.375 UTC
testTime :: UTCTime
testTime = UTCTime (ModifiedJulianDay 59724)
                   (realToFrac (378995 % 8 :: Rational))

testYear :: Day
testYear =
    let (y,_,_) = toGregorian (utctDay testTime)
    in fromGregorian y 1 1


dateTimeUnit :: Spec
dateTimeUnit =
  describe "Date and time Param types" $ do
    it "UTCTime is correctly encoded" $
      render testTime `shouldBe` Plain "'2022-05-25 13:09:34.375'"
    it "Day is correctly encoded" $
      render (utctDay testTime) `shouldBe` Plain "'2022-05-25'"
    it "TimeOfDay is correctly encoded" $
      render (TimeOfDay 13 9 34.375) `shouldBe` Plain "'13:09:34.375'"


dateTimeSpec :: Connection -> Spec
dateTimeSpec conn =
  beforeAll_
    ( do  _ <- execute_ conn "drop table if exists datetime"
          _ <- execute_ conn ( "create table datetime "
                               <> "(i int,"
                               <> " dt datetime(6),"
                               <> " d date,"
                               <> " y year,"
                               <> " ts timestamp(6),"
                               <> " t time(6))"
                             )
          void $ execute_ conn (
                   "insert into datetime (i,dt,d,y,ts,t) values "
                   <> "(1,'2022-05-25 13:09:34.375','2022-05-25','2022','2022-05-25 13:09:34.375','13:09:34.375'),"
                   <> "(2,'0000-00-00 00:00:00','0000-00-00','0000','0000-00-00 00:00:00','00:00:00')"
                 )
    ) $ do
      describe "reading date and time" $ do
        it "should read DATETIME correctly" $ do
          result <- query_ conn "select dt from datetime where i = 1"
          result `shouldBe` [Only testTime]
        it "should read DATE correctly" $ do
          result <- query_ conn "select d from datetime where i = 1"
          result `shouldBe` [Only $ utctDay testTime]
        it "should read YEAR correctly" $ do
          result <- query_ conn "select y from datetime where i = 1"
          result `shouldBe` [Only testYear]
        it "should read TIMESTAMP correctly" $ do
          result <- query_ conn "select ts from datetime where i = 1"
          result `shouldBe` [Only testTime]
        it "should read TIME correctly" $ do
          result <- query_ conn "select t from datetime where i = 1"
          result `shouldBe` [Only $ TimeOfDay 13 9 34.375]
        it "should be vaguely reasonable given a zero DATETIME" $ do
          result <- query_ conn "select dt from datetime where i = 2"
          result `shouldBe` [Only $ UTCTime (fromGregorian 0 0 0) 0]
        it "should be vaguely reasonable given a zero DATE" $ do
          result <- query_ conn "select d from datetime where i = 2"
          result `shouldBe` [Only $ fromGregorian 0 0 0]
        it "should be vaguely reasonable given a zero YEAR" $ do
          result <- query_ conn "select y from datetime where i = 2"
          result `shouldBe` [Only $ fromGregorian 0 0 0]
