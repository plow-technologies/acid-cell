{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

module Data.Acid.Cell.InternalSpec (main
                                   , KeyedTestSet(..)
                                   , TestKey (..) 
                                   
                                   , TestHost (..) 
                                    
                                   , KeyedTestSetStore (..)
                                   , AcidCellTestSet (..)
                                   , TestSource (..)
                                   , TestHost (..)
                                   , TestTime (..) 
                                   , newKeyedTestSetStore
                                   , TestDest (.. )
                                   , TestSource (.. )
                                   , getTestCellKey
                                    ,spec ) where 


import CorePrelude
import Control.Lens
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )
import Test.Hspec
import GHC.Generics
import qualified Data.Serialize as Ser

import Data.Typeable
import qualified Data.Set as S


import Data.SafeCopy        ( base, deriveSafeCopy )


-- Texty Text
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
-- PlowSpecific
import DirectedKeys
import DirectedKeys.Types
import Data.Acid.Cell.Types


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "KeyedTestSet" $ do
    it "should provide sample data to make Acidic" $ do
      True `shouldBe` True



getTestCellKey :: CellKey TestKey TestSource TestDest TestTime KeyedTestSetStore 
getTestCellKey = CellKey tGetKey tCodeCellKeyFilename tDecodeCellKeyFilename
    where                            -- No type synonym use, because this is going to acid state stuff
      tGetKey :: KeyedTestSetStore -> (DirectedKeyRaw TestKey TestHost TestHost TestTime)
      tGetKey = testSetKey.getKeyedTestSet 
      tCodeCellKeyFilename ::  (DirectedKeyRaw TestKey TestHost TestHost TestTime) -> Text
      tCodeCellKeyFilename = T.decodeUtf8 . parseFilename . encodeKey
      tDecodeCellKeyFilename :: Text -> Either Text (DirectedKeyRaw TestKey TestHost TestHost TestTime)
      tDecodeCellKeyFilename t = over _Left vswap (decodeKeyPart.decodeFilename.T.encodeUtf8 $ t)
      vswap = T.pack -- change the string to text

-- the generic is important for creating Serialize

newtype KeyedTestSetStore = KeyedTestSetStore { getKeyedTestSet :: KeyedTestSet AcidCellTestKey (S.Set Text) } 
    deriving (Eq,Ord,Generic,Typeable)

data KeyedTestSet k a = KeyedTestSet { 
        testSetKey :: k
      , testSetData :: a
      }deriving (Eq,Ord,Generic)



newtype TestKey = TestKey {unKey:: Int } 
    deriving (Eq,Show,Ord,Generic) 

instance Ser.Serialize TestKey where


newtype TestHost = TestHost { unHost :: Int } 
    deriving (Eq,Show,Ord,Generic) 

instance Ser.Serialize TestHost where

newtype TestTime = TestTime { unTime :: Int} 
    deriving (Eq,Show,Ord,Generic) 

instance Ser.Serialize TestTime where


type TestSource = TestHost 
type TestDest   = TestHost 


-- All the types in a DirectedKey Should be boxed
type AcidCellTestKey = DirectedKeyRaw TestKey TestHost TestDest TestTime

-- Here is our complete type 
type AcidCellTestSet = KeyedTestSet AcidCellTestKey (S.Set Text)



-- SafeCopy instances for serialization 

$(deriveSafeCopy 0 'base ''TestTime)
$(deriveSafeCopy 0 'base ''TestHost)
$(deriveSafeCopy 0 'base ''TestKey)
$(deriveSafeCopy 0 'base ''KeyedTestSet)
$(deriveSafeCopy 0 'base ''KeyedTestSetStore)
$(deriveSafeCopy 0 'base ''DirectedKeyRaw)




-- Acid State DIG

-- Intializing Function
newKeyedTestSetStore = KeyedTestSetStore . KeyedTestSet k $ S.empty
    where k = DKeyRaw  (TestKey 0)  (TestHost 0)  (TestHost 0)  (TestTime 0)
          e :: Text 
          e = ""



-- Lensing is a nice thing to do here, but this is the quick and dirty way

insertNew :: Text -> Update (KeyedTestSetStore ) Int 
insertNew t = do 
  c@(KeyedTestSetStore {getKeyedTestSet = (ts@(KeyedTestSet {testSetKey=tsk, testSetData = tsd}))}) <- get 
  let newSet = S.insert t tsd 
  put $ c { getKeyedTestSet= ts{testSetData=newSet}}
  return . S.size $ newSet


$(makeAcidic ''KeyedTestSetStore ['insertNew])
