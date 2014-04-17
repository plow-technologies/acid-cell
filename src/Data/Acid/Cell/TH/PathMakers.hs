{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

module Data.Acid.Cell.TH.PathMakers ( 
                              -- allPathMakers,
                              -- allStoreNames,
                              -- istorename,
                              -- dstorename,
                              -- gstorename
                            ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics
import Data.Typeable

import Data.Acid.Cell.Types
-- | Build the Path Acid State function for make Acidic 

-- allPathMakers = [deletePathMaker,insertPathMaker,getPathMaker]
-- allStoreNames = [dstorename,istorename,gstorename]


-- -- |'insertPathMaker' returns an Acidic insert constructor funtion named Insert<NameOfData>CellPath
-- --  'istorename' is its extracted naming engine for use in makeAcidic Cell

-- insertPathMaker :: Name -> Name -> Q Dec
-- insertPathMaker ck storename = do 
--   f <- (funD (istorename storename)) [(clause [] (normalB insertAcidCellPathTH ) [] )]
--   return f
--                                    where 
--       insertAcidCellPathTH = appE (varE 'insertAcidCellPath) (varE ck)
      
-- istorename :: Name -> Name
-- istorename storename = mkName.concat $ ["insert" ,(nameBase storename) , "CellPath"]




-- -- |'gPathMaker' returns an Acidic insert constructor funtion named Get<NameOfData>CellPath
-- --  'gstorename' is its extracted naming engine for use in makeAcidic Cell

-- getPathMaker :: Name -> Name -> Q Dec 
-- getPathMaker ck storename = do 
--   f <- (funD (gstorename storename)) [(clause [] (normalB getAcidCellPathTH) [])]
--   return f 
--       where
--         getAcidCellPathTH = appE (varE 'getAcidCellPath) (varE ck)
-- gstorename :: Name -> Name
-- gstorename storename = mkName.concat $ ["get" ,(nameBase storename) , "CellPath"]


-- -- |'deletePathMaker' returns an Acidic insert constructor funtion named Insert<NameOfData>CellPath
-- --  'istorename' is its extracted naming engine for use in makeAcidic Cell

-- deletePathMaker :: Name -> Name -> Q Dec
-- deletePathMaker ck storename = do 
--   f <- (funD (dstorename storename)) [(clause [] (normalB deleteAcidCellPathTH ) [] )]
--   return f
--                                    where 
--       deleteAcidCellPathTH = appE (varE 'deleteAcidCellPath) (varE ck)
      
-- dstorename :: Name -> Name
-- dstorename storename = mkName.concat $ ["delete" ,(nameBase storename) , "CellPath"]
