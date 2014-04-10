{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

module Data.Acid.Cell.InternalSpec (main
                                   , KeyedTestSet(..)
                                   , TestKey (..) 
                                   
                                   , TestHost (..) 
                                    
                                    
                                   , AcidCellTestSet (..)
                                   , TestSource (..)
                                   , TestHost (..)
                                   , TestTime (..) 
                                   , newKeyedTestSetStore
                                    ,spec ) where 


import CorePrelude
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )
import Test.Hspec
import GHC.Generics
import Data.Typeable
import qualified Data.Set as S
import DirectedKeys.Types
import Data.SafeCopy        ( base, deriveSafeCopy )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "KeyedTestSet" $ do
    it "should provide sample data to make Acidic" $ do
      True `shouldBe` True


-- the generic is important for creating Serialize

newtype KeyedTestSetStore = KeyedTestSetStore { getKeyedTestSet :: KeyedTestSet AcidCellTestKey (S.Set Text) } 
    deriving (Eq,Ord,Generic)

data KeyedTestSet k a = KeyedTestSet { 
        testSetKey :: k
      , testSetData :: a
      }deriving (Eq,Ord,Generic)



newtype TestKey = TestKey {unKey:: Int } 
    deriving (Eq,Show,Ord,Generic) 

newtype TestHost = TestHost { unHost :: Int } 
    deriving (Eq,Show,Ord,Generic) 
newtype TestTime = TestTime { unTime :: Int} 
    deriving (Eq,Show,Ord,Generic) 

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
