{-# LANGUAGE OverloadedStrings,NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{-| 

    This module defines the types used in the Template haskell routine in order to automate the creation of a
    higher level set of access functions to the Atomic Data in Acid State.

    Most notably, it allows datatypes that look like Keyed vectors to make changes without write locking above the Key Level

    This is very important when writing to Time Series Data.  
    
|-} 

module Data.Acid.Cell.Types () where 

-- Controls
import CorePrelude
import Control.Concurrent
import Control.Concurrent.STM


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
                                  , decodeCellKeyFilename :: Text -> (DirectedKeyRaw  k src dst tm)
                                  }

-- | the codeCellKeyFilename actually will be used to make these file keys but
-- I figure why not just make it where you can pass a Text generator.

newtype FileKey = FileKey { getFileKey :: Text} deriving (Show,Generic,Typeable)
    
$(deriveSafeCopy 0 'base ''FileKey)


-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore  k src dst tm tvlive stdormant = CellCore { 
      ccLive :: (M.Map (DirectedKeyRaw  k src dst tm) tvlive )
      ,ccDormant :: (M.Map FileKey stdormant )
    }

-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for 
--  dormant filenames

type TCellCore  k src dst tm stlive stdormant = TVar (CellCore  k src dst tm (TVar stlive) stdormant)


data AcidCell  k src dst tm stlive stdormant = AcidCell { 
      cellCore :: TCellCore  k src dst tm stlive stdormant 
    , cellKey  :: CellKey  k src dst tm stdormant -- stdormant here because that is the actual acid state representation 
    } 




-- |Cell Core interaction functions 
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Acidic Core


-- | Path manipulation happens at every atomic change to the Cell
-- These functions are made acidic 
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it


-- | DIG interface
deleteAcidCellPath:: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant -> (DirectedKeyRaw  k src dst tm) ->  Update stdormant FileKey
deleteAcidCellPath = undefined 


insertAcidCellPath :: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant ->  FileKey ->  Update stdormant (AEither FileKey)
insertAcidCellPath = undefined 

getAcidCellPath :: (Typeable stdormant, IsAcidic stdormant) => CellKey  k src dst tm stdormant  -> FileKey ->  Query stdormant (AEither FileKey)
getAcidCellPath = undefined






-- | User Interface Defining Functions

-- | The 'st' in the type definition here is the AcidState that will be turned into a 
insertState :: (Typeable st, IsAcidic st) => 
               (AcidCell  k src dst tm stlive stdormant)  -> st -> IO (AEither (DirectedKeyRaw  k src dst tm))
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
                       t st -> IO (AcidCell k src dst tm st stdormant)
initializeAcidCell = undefined


-- | Exception and Error handling
type AEither a = Either AcidCellError a

data AcidCellError   = InsertFail    Text 
                     | DeleteFail    Text
                     | StateNotFound Text



                        
