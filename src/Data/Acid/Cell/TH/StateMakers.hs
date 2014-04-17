{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{- | StateMakers are TH functions that assemble the insert state, delete state and update state pieces of a live DB
     entity. 

|-}
module Data.Acid.Cell.TH.StateMakers ( 
                                      allStateMakers
                            ) where



import Language.Haskell.TH


import Data.Acid.Cell.Types



type CellKeyName = Name 
type InitializerName = Name 
type StateName = Name 


allStateMakers :: [CellKeyName -> InitializerName -> StateName -> Q Dec]
allStateMakers = [ makeInitializeXAcidCell
                 , makeInsertXAcidCell
                 , makeDeleteXAcidCell
                 , makeGetXAcidCell
                 , makeFoldlWithKeyXAcidCell
                 ]
-- The X represents the position of the incoming type in the filename 
makeInitializeXAcidCell ::  CellKeyName -> InitializerName -> StateName -> Q Dec
makeInitializeXAcidCell ckN initN stN = do 
  f <- (funD (buildInitName stN)) [(clause [] (normalB initializeAcidCellTH ) [] ) ]
  return f
  where 
    initializeAcidCellTH = appE (appE (varE 'initializeAcidCell ) (varE ckN)) (varE initN) 

buildInitName :: StateName -> Name
buildInitName stN = mkName.concat $ ["initialize",(nameBase stN), "AC"]

makeInsertXAcidCell ::  CellKeyName -> InitializerName -> StateName -> Q Dec
makeInsertXAcidCell ckN initN stN = do 
  f <- (funD (buildInsertName stN)) [(clause [] (normalB insertAcidCellTH) [] ) ] 
  return f 
  where 
    insertAcidCellTH = appE (appE (varE 'insertState ) (varE ckN)) (varE initN)

buildInsertName :: StateName -> Name
buildInsertName stN = mkName.concat $ ["insert", (nameBase stN), "AC"]
  

makeDeleteXAcidCell ::  CellKeyName -> InitializerName -> StateName -> Q Dec
makeDeleteXAcidCell ckN _ stN = do 
  f <- (funD (buildDeleteName stN)) [(clause [] (normalB deleteAcidCellTH) [] ) ] 
  return f 
  where 
    deleteAcidCellTH =  (appE (varE 'deleteState ) (varE ckN)) 

buildDeleteName :: StateName -> Name 
buildDeleteName stN = mkName.concat $ ["delete", (nameBase stN), "AC"]
  


makeGetXAcidCell :: CellKeyName -> InitializerName -> StateName -> Q Dec
makeGetXAcidCell ckN _ stN = do 
  f <- (funD (buildGetName stN)) [(clause [] (normalB getAcidCellTH) [] ) ] 
  return f 
  where 
    getAcidCellTH = (appE (varE 'getState ) (varE ckN)) 

buildGetName :: StateName -> Name
buildGetName stN = mkName.concat $ ["get", (nameBase stN), "AC"]
  


makeFoldlWithKeyXAcidCell ::  CellKeyName -> InitializerName -> StateName -> Q Dec
makeFoldlWithKeyXAcidCell ckN _ stN = do 
  f <- (funD (buildFoldlWithKeyName stN)) [(clause [] (normalB foldlWithKeyAcidCellTH) [] ) ] 
  return f 
  where 
    foldlWithKeyAcidCellTH = (appE (varE 'stateFoldlWithKey ) (varE ckN)) 

buildFoldlWithKeyName :: StateName -> Name
buildFoldlWithKeyName stN = mkName.concat $ ["foldlWithKey", (nameBase stN), "AC"]
  
