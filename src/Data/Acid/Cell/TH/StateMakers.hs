{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{- | StateMakers are TH functions that assemble the insert state, delete state and update state pieces of a live DB
     entity. 

|-}
module Data.Acid.Cell.TH.StateMakers ( 
                                      allStateMakers
                            ) where



import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics
import Data.Typeable

import Data.Acid.Cell.Types


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



-- initializeStateMaker :: Name -> Name -> Q Dec 
-- initializeStateMaker ck storename = do 
--  (funD (ikName)) [(clause [] (normalB initializeKeyedTestStoreTH) [] )]
--  where 
--    initializeKeyedTestStoreTH = doE stmts
--     = 
  


type CellKeyName = Name 
type InitializerName = Name 
type StateName = Name 



allStateMakers = [ makeInitializeXAcidCell
                 , makeInsertXAcidCell
                 , makeDeleteXAcidCell
                 , makeGetXAcidCell
                 , makeFoldlWithKeyXAcidCell
                 ]
-- The X represents the position of the incoming type in the filename 
makeInitializeXAcidCell ckN initN stN = do 
  f <- (funD (buildInitName stN)) [(clause [] (normalB initializeAcidCellTH ) [] ) ]
  return f
  where 
    initializeAcidCellTH = appE (appE (varE 'initializeAcidCell ) (varE ckN)) (varE initN) 

buildInitName stN = mkName.concat $ ["initialize",(nameBase stN), "AC"]


makeInsertXAcidCell ckN initN stN = do 
  f <- (funD (buildInsertName stN)) [(clause [] (normalB insertAcidCellTH) [] ) ] 
  return f 
  where 
    insertAcidCellTH = appE (appE (varE 'insertState ) (varE ckN)) (varE initN)

buildInsertName stN = mkName.concat $ ["insert", (nameBase stN), "AC"]
  


makeDeleteXAcidCell ckN initN stN = do 
  f <- (funD (buildDeleteName stN)) [(clause [] (normalB deleteAcidCellTH) [] ) ] 
  return f 
  where 
    deleteAcidCellTH =  (appE (varE 'deleteState ) (varE ckN)) 

buildDeleteName stN = mkName.concat $ ["delete", (nameBase stN), "AC"]
  



makeGetXAcidCell ckN initN stN = do 
  f <- (funD (buildGetName stN)) [(clause [] (normalB getAcidCellTH) [] ) ] 
  return f 
  where 
    getAcidCellTH = (appE (varE 'getState ) (varE ckN)) 

buildGetName stN = mkName.concat $ ["get", (nameBase stN), "AC"]
  



makeFoldlWithKeyXAcidCell ckN initN stN = do 
  f <- (funD (buildFoldlWithKeyName stN)) [(clause [] (normalB foldlWithKeyAcidCellTH) [] ) ] 
  return f 
  where 
    foldlWithKeyAcidCellTH = (appE (varE 'stateFoldlWithKey ) (varE ckN)) 

buildFoldlWithKeyName stN = mkName.concat $ ["foldlWithKey", (nameBase stN), "AC"]
  
