{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies, NoMonomorphismRestriction,DeriveDataTypeable #-}

module Data.Acid.Cell.TypesSpec (main, spec) where
-- import Data.Acid.Cell.InternalSpec hiding (main,spec)
import Test.Hspec
import Data.Acid            ( AcidState, Query, Update, EventResult
                            , makeAcidic,openLocalStateFrom,  closeAcidState )

import Data.Acid.Local (createCheckpointAndClose,createLocalArchive)
import Filesystem 

import Language.Haskell.TH
-- Control

import CorePrelude

import Control.Concurrent.STM
import Data.Foldable
import Data.Traversable


-- Containers 
import qualified Data.Set as S
import qualified Data.Map as M
-- Texty Text
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
-- PlowSpecific
import DirectedKeys
import DirectedKeys.Types

-- Local
import Data.Acid.Cell.InternalSpec hiding (main,spec)
import Data.Acid.Advanced   (query', update')
import Data.Acid.Cell.Types
import Data.Acid.Cell.TH

{-| A few weird tests going on here, notably we are reifying some functions to make the acid store that will normally be made 
by template haskell, there are restrictions in Update about what can and can't be made into acid state so I have to use the 
Test stages to create Acid States for test without them.

This is why the test data in Internal was made.
|-}

$(makeAcidCell 'getTestCellKey 'initKeyedTestSetStore ''KeyedTestSetStore)

stressTestFD = do
  ac <- initializeKeyedTestSetStoreAC "testFD"
  print "testing file descriptor overload"
  sts <- traverse ((insertKeyedTestSetStoreAC ac).newKeyedTestSetStore)  [1 .. 20]
  -- stbacks <- getKeyedTestSetStoreAC ac (newKeyedTestSetStore 1)
  -- st3 <- case st2 of 
  --          Nothing ->  (insertKeyedTestSetStoreAC ac (newKeyedTestSetStore 1 ) )
  --          Just st ->  return st
  archiveAndHandleKeyedTestSetStoreAC ac (\_ b -> return b)
  createCheckpointAndCloseKeyedTestSetStoreAC ac 

--  deleteKeyedTestSetStoreAC ac (newKeyedTestSetStore 1)
--  traverseWithKeyKeyedTestSetStoreAC ac (\ck dr ast -> fail "testFailure")



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stressTestFD " $ do
    it "should insert states until at least 2000 and work" $ do
      stressTestFD
      True `shouldBe` False




{-| 

       The pattern I would like to follow for creating functions is take the word 'AcidCell' and replace it with an actual type 
       as.
|-}


type TestCellKey = CellKey TestKey TestSource TestDest TestTime KeyedTestSetStore 
type TestCellDirectedKey = (DirectedKeyRaw TestKey TestHost TestHost TestTime)


-- insertTestCellPath :: KeyedTestSetStore  -> Update CellKeyStore FileKey
-- insertTestCellPath tck fAcid = insertAcidCellPath  getTestCellKey fAcid

-- testControlFlow = do 
--   acidCell <- initializeAcidCell getTestCellKey newKeyedTestSetStore "testBS"
--   insertState getTestCellKey newKeyedTestSetStore acidCell newKeyedTestSetStore
--   stuff <- getState getTestCellKey acidCell newKeyedTestSetStore
--   deleteState getTestCellKey acidCellnewKeyedTestSetStore 

--   case stuff of 
--     Nothing -> print "wheee" 
--     Just _ -> print "whoah"


-- | Stress test should be run twice to get everything in state

testControlFlowNew = do   
  wd <- getWorkingDirectory
  ac <- initializeKeyedTestSetStoreAC "testBS"
  print "i1"
  st1 <- insertKeyedTestSetStoreAC ac (newKeyedTestSetStore 1)
  st2 <- getKeyedTestSetStoreAC ac (newKeyedTestSetStore 1)
  st3 <- case st2 of 
           Nothing ->  (insertKeyedTestSetStoreAC ac (newKeyedTestSetStore 1 ) )
           Just st ->  return st
  archiveAndHandleKeyedTestSetStoreAC ac (\_ b -> return b)
  deleteKeyedTestSetStoreAC ac (newKeyedTestSetStore 1)
  createCheckpointAndCloseKeyedTestSetStoreAC ac 
  traverseWithKeyKeyedTestSetStoreAC ac (\ck dr ast -> fail "testFailure")
  setWorkingDirectory wd 
  return st3
  
-- $(buildInsertXCellPath 'getTestCellKey ''CellKeyStore)
-- $(buildInsertXCellPath 'getTestCellKey ''KeyedTestSetStore)

-- | It is always the ''CellKeyStore that gets made acidic, 
-- the wrapper above is to reify the CellKey in order for this to be possible

-- $(makeAcidicCell ''KeyedTestSetStore)
 
-- | foldlM :: (Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a

-- initializeKeyedTestSetStoreState ck emptyTargetState root = do
--   st <- openLocalStateFrom root emptyCellKeyStore
--   fkSet <- query' st (GetKeyedTestSetStoreCellPath)
--   let setEitherDrRaw = S.map ((decodeCellKeyFilename ck).getFileKey) fkSet
--   stateMap <- foldlM foldMFcn  M.empty setEitherDrRaw  
--   tcc <- newTVarIO (CellCore stateMap st )
--   return $ AcidCell tcc ck
--    where 
--      foldMFcn  cellMap (Left _) = return cellMap 
--      foldMFcn  cellMap (Right fk) = do 
--        st' <- openLocalStateFrom root emptyTargetState 
--        return $ M.insert fk st' cellMap 
