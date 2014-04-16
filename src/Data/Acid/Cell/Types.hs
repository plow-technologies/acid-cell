{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{-| 

    This module defines the types used in the Template haskell routine in order to automate the creation of a
    higher level set of access functions to the Atomic Data in Acid State.

    Most notably, it allows datatypes that look like Keyed vectors to make changes without write locking above the Key Level

    This is very important when writing to Time Series Data.  
    
|-} 

module Data.Acid.Cell.Types (AcidCellError (..)
                            , CellKey (..)
                            , getKey
                            , codeCellKeyFilename
                            , decodeCellKeyFilename                              
                            , AcidCell 
                            , cellCore
                            , cellKey
                            , FileKey (..)
                            , CellKeyStore
                            , deleteAcidCellPath
                            , insertAcidCellPath
                            , getAcidCellPath
                            ) where

-- Controls
import CorePrelude
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader ( ask )
import Control.Monad.State  
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
-- Typeclasses
import Data.Acid
import Data.Typeable
import Data.Foldable
import Data.Traversable
import GHC.Generics
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)

-- Component Libraries
import DirectedKeys.Types

-- Containers 
import qualified Data.Map as M 
import qualified Data.Set as S

-- | 'CellKey' declares two functions, 
-- 'getKey' which is supposed to take an Acidic Value and return a 'DirectedKeyRaw' 
-- 'makeFilename' which takes a directed key and returns text.  
-- you could use 'parseFilename' which escapes all the characters with valid unix ones
-- or you can write your own, but it is very important that they are invertable!
-- so be careful


-- | 'CellKey' enforces no guarantees on your filenames, this is done because
-- we want to keep the libraries light and portable but it means you need to add these 
-- guarantees on your functions 

data CellKey k src dst tm st = CellKey { getKey :: st -> (DirectedKeyRaw k src dst tm)
                                  , codeCellKeyFilename :: (DirectedKeyRaw  k src dst tm) -> Text
                                  , decodeCellKeyFilename :: Text -> Either Text (DirectedKeyRaw  k src dst tm)
                                  }

-- | the codeCellKeyFilename actually will be used to make these file keys but
-- I figure why not just make it where you can pass a Text generator.


newtype FileKey = FileKey { getFileKey :: Text} deriving (Show,Generic,Typeable,Ord,Eq)


$(deriveSafeCopy 0 'base ''FileKey)

makeFileKey :: CellKey k src dst tm st -> st -> FileKey 
makeFileKey ck s = FileKey (codeCellKeyFilename ck . getKey ck $ s)


-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore  k src dst tm tvlive stdormant = CellCore { 
      ccLive     :: (M.Map (DirectedKeyRaw  k src dst tm) tvlive )
      ,ccDormant :: CellKeyStore
    }

newtype CellKeyStore  = CellKeyStore { getCellKeyStore :: (S.Set FileKey)}
    deriving (Typeable,Show,Generic)


$(deriveSafeCopy 0 'base ''CellKeyStore)

-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for 
--  dormant filenames

type TCellCore  k src dst tm stlive stdormant = TVar (CellCore  k src dst tm (TVar stlive) stdormant)


data AcidCell  k src dst tm stlive stdormant = AcidCell { 
      cellCore :: TCellCore  k src dst tm stlive stdormant 
    , cellKey  :: CellKey  k src dst tm stdormant 
      -- stdormant here because that is the actual acid state representation 
    } 
   deriving (Typeable,Generic)



-- |Cell Core interaction functions 
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Acidic Core


-- | Path manipulation happens at every atomic change to the Cell
-- These functions are made acidic 
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it

-- | DIG interface

deleteAcidCellPath:: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant ->  stdormant ->  Update CellKeyStore FileKey
deleteAcidCellPath ck st = do 
  cks@(CellKeyStore { getCellKeyStore = hsSet}) <- get 
  (void $ put $ (CellKeyStore (S.delete  fk hsSet )))
  return fk
         where 
           fk = (makeFileKey ck st)
  


insertAcidCellPath :: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant ->  stdormant ->  Update CellKeyStore FileKey
insertAcidCellPath ck st =  do 
  cks@(CellKeyStore { getCellKeyStore = hsSet}) <- get 
  (void $ put $ (CellKeyStore (S.insert  fk hsSet )))
  return fk
         where 
           fk = (makeFileKey ck st)

  

getAcidCellPath :: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant  ->  stdormant ->  Query CellKeyStore ((S.Set FileKey))
getAcidCellPath ck st = do
  cks@(CellKeyStore { getCellKeyStore = hsSet}) <- ask
  case hsSet of 
    _
        |S.null hsSet -> return  S.empty
        | otherwise -> do 
               return  hsSet
      where 
        fk = (makeFileKey ck st)
  
  


-- | User Interface Defining Functions

-- | The 'st' in the type definition here is the AcidState that will be turned into a 

insertState :: (Typeable st, IsAcidic st) =>  (AcidCell  k src dst tm stlive stdormant)  -> st -> IO (AEither (DirectedKeyRaw  k src dst tm))
insertState = undefined 


deleteState :: (Typeable st, IsAcidic st) => 
               (AcidCell  k src dst tm stlive stdormant)  -> st -> IO (AEither Bool)
deleteState = undefined


getState :: (AcidCell  k src dst tm stlive stdormant) ->  (DirectedKeyRaw  k src dst tm) ->  IO (AEither stlive)
getState = undefined


queryCells ::(Monoid (f a)) => 
             (AcidCell  k src dst tm stlive stdormant) ->  (DirectedKeyRaw  k src dst tm) ->  IO (AEither (f stlive))
queryCells = undefined 

-- | Initialization 
initializeAcidCell ::  (Typeable stdormant, IsAcidic stdormant , Typeable st ,IsAcidic st,Traversable t  ) => 
                       FilePath -> 
                       CellKey k src dst tm stdormant -> 
                       t st -> 
                       IO (AcidCell k src dst tm st stdormant)

initializeAcidCell = undefined

-- | Exception and Error handling
type AEither a = Either AcidCellError a

data AcidCellError   = InsertFail    Text 
                     | DeleteFail    Text
                     | StateNotFound Text
