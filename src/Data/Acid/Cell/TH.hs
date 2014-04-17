{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{-| 
    This module defines the template haskell interface to Data.Cell which actually generates the types we will use
|-} 

module Data.Acid.Cell.TH ( 
                          makeAcidCell

                            ) where
--Meta 
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

import GHC.Generics
import Data.Typeable

-- Controls
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader ( ask )
import Control.Monad.State  
import Control.Applicative

-- Typeclasses
import Data.Acid

import Data.Traversable
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)

-- Component Libraries
import Data.Acid.Cell.TH.PathMakers
import Data.Acid.Cell.TH.StateMakers
import DirectedKeys.Types

-- Containers 
import qualified Data.Map as M 
import qualified Data.Set as S


-- Local 
import Data.Acid.Cell.Types

makeAcidCell :: Name -> Name -> Name ->  Q [Dec]
makeAcidCell ckN initN stN = do 
  Data.Traversable.sequence $ allStateMakers <*> [ckN] <*> [initN] <*> [stN]

-- buildInsertXCellPath :: Name -> Name -> Q [Dec]
-- buildInsertXCellPath ck storename  = do
--   Data.Traversable.sequence $ allPathMakers  <*> [ck] <*> [storename]

-- -- | because of the template haskell staging restriction we need to wrap $(makeAcidic) in this function 

-- makeAcidicCell :: Name -> Q [Dec]
-- makeAcidicCell storename = do 
--   (Loc _ _ md _ _) <- location
--   qdecs <- makeAcidic ''CellKeyStore (acidPathMaker md storename)
--   return $  qdecs


-- acidPathMaker ::  String -> Name -> [Name] 
-- acidPathMaker md storename = modAppIname
--     where 
--       mPfx = (\strName -> md ++ "." ++ strName)
--       fNameMake :: (Name->Name) -> Name -> Name 
--       fNameMake nameMakerFcn= (mkName . mPfx . nameBase).nameMakerFcn
--       modAppIname :: [Name] 
--       modAppIname =  fNameMake <$> allStoreNames <*> [storename]
