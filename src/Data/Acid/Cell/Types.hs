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

data CellKey k h s t st = CellKey { getKey :: st -> (DirectedKeyRaw k h s t)
                                  , codeCellKeyFilename :: (DirectedKeyRaw k h s t) -> Text
                                  , decodeCellKeyFilename :: Text -> (DirectedKeyRaw k h s t)
                                  }


-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore k h s t stlive stdormant = CellCore { 
      ccLive :: (M.Map (DirectedKeyRaw k h s t) stlive )
      ,ccDormant :: (M.Map (DirectedKeyRaw k h s t) stdormant )
    }

-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for 
--  dormant filenames

type TCellCore k h s t stlive stdormant = TVar (CellCore k h s t (TVar stlive) (TVar stdormant))


data AcidCell k h s st stlive stdormant = AcidCell { 
      cellCore :: TCellCore k h s st stlive stdormant 
    , cellKey  :: CellKey k h s s st 
    } 


insertState :: (Typeable st, IsAcidic st) => (AcidCell k h s st stlive stdormant)  -> st -> IO (Either (AcidCellError Text) (DirectedKeyRaw k h s st))
insertState = undefined 

deleteState :: (Typeable st, IsAcidic st) => (AcidCell k h s st stlive stdormant)  -> st -> IO (Either (AcidCellError Text) Bool)
deleteState = undefined

getState :: (AcidCell k h s st stlive stdormant) ->  (DirectedKeyRaw k h s st) ->  IO (Either (AcidCellError Text) stlive)
getState = undefined


data AcidCellError a = InsertFail    a 
                     | DeleteFail    a
                     | StateNotFound a



                        
