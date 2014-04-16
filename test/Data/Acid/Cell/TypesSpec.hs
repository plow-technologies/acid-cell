{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies, DeriveDataTypeable #-}

module Data.Acid.Cell.TypesSpec (main, spec) where
-- import Data.Acid.Cell.InternalSpec hiding (main,spec)
import Test.Hspec
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )


import Language.Haskell.TH
-- Control
import Control.Lens
import CorePrelude
-- Texty Text
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
-- PlowSpecific
import DirectedKeys
import DirectedKeys.Types

-- Local
import Data.Acid.Cell.InternalSpec hiding (main,spec)
import Data.Acid.Cell.Types
import Data.Acid.Cell.TH

{-| A few weird tests going on here, notably we are reifying some functions to make the acid store that will normally be made 
by template haskell, there are restrictions in Update about what can and can't be made into acid state so I have to use the 
Test stages to create Acid States for test without them.

This is why the test data in Internal was made.
|-}





main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False




{-| 

       The pattern I would like to follow for creating functions is take the word 'AcidCell' and replace it with an actual type 
       as.
jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj jj
   jj jj jj jj jj jj jj jj jj jj jj jj jj    jj jj jj jj jj jj jj jj jj jj jj jj
      jj jj jj jj jj jj jj jj jj jj jj          jj jj jj jj jj jj jj jj jj jj
         jj jj jj jj jj jj jj jj jj                jj jj jj jj jj jj jj jj
            jj jj jj jj jj jj jj                      jj jj jj jj jj jj
               jj jj jj jj jj                            jj jj jj jj
                  jj jj jj                                  jj jj
                     jj                                      j j
|-}


type TestCellKey = CellKey TestKey TestSource TestDest TestTime KeyedTestSetStore 
type TestCellDirectedKey = (DirectedKeyRaw TestKey TestHost TestHost TestTime)

-- getTestCellKey :: CellKey TestKey TestSource TestDest TestTime KeyedTestSetStore 
-- getTestCellKey = CellKey tGetKey tCodeCellKeyFilename tDecodeCellKeyFilename
--     where                            -- No type synonym use, because this is going to acid state stuff

--       tGetKey :: KeyedTestSetStore -> (DirectedKeyRaw TestKey TestHost TestHost TestTime)
--       tGetKey = testSetKey.getKeyedTestSet 

--       tCodeCellKeyFilename ::  (DirectedKeyRaw TestKey TestHost TestHost TestTime) -> Text
--       tCodeCellKeyFilename = T.decodeUtf8 . parseFilename . encodeKey

--       tDecodeCellKeyFilename :: Text -> Either Text (DirectedKeyRaw TestKey TestHost TestHost TestTime)
--       tDecodeCellKeyFilename t = over _Left vswap (decodeKeyPart.decodeFilename.T.encodeUtf8 $ t)

--       vswap = T.pack -- change the string to text


insertTestCellPath :: KeyedTestSetStore  -> Update CellKeyStore FileKey
insertTestCellPath tck = insertAcidCellPath getTestCellKey tck

$(buildInsertXCellPath 'getTestCellKey ''CellKeyStore)

$(makeAcidicCell ''CellKeyStore)
 
