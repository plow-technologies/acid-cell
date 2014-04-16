
{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Acid.Cell.THSpec (main, spec) where
-- import Data.Acid.Cell.InternalSpec hiding (main,spec)
import Test.Hspec
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )
import CorePrelude
import Data.Acid.Cell.Types


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False




