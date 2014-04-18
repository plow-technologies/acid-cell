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
                            , CellKeyStore
                            , AcidCell
                            , DeleteAcidCellPathFileKey
                            , InsertAcidCellPathFileKey
                            , initializeAcidCell
                            , getState
                            , insertState
                            , deleteState
                            , stateFoldlWithKey
                            ) where


-- System 
import Filesystem.Path.CurrentOS hiding (root)
import Filesystem 

-- Controls
import CorePrelude
import Control.Concurrent.STM
import Control.Monad.Reader ( ask )
import Control.Monad.State  

-- Typeclasses
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose,createLocalArchive)
import Data.Acid.Advanced   (update', query')

import Data.Foldable
import Data.Traversable

import GHC.Generics
import Data.SafeCopy        (SafeCopy,base, deriveSafeCopy)

-- Component Libraries
import DirectedKeys.Types

-- Containers 
import qualified Data.Map as M 
import qualified Data.Set as S

-- Strings /Monomorphs 
import qualified Data.Text as T

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

unmakeFileKey :: CellKey k src dst tm st
                       -> FileKey -> Either Text (DirectedKeyRaw k src dst tm)
unmakeFileKey ck s = (decodeCellKeyFilename ck).getFileKey $ s

-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore  k src dst tm tvlive stdormant = CellCore { 
       ccLive     :: TVar (M.Map (DirectedKeyRaw  k src dst tm) tvlive )
      ,ccDormant :: stdormant
    }

newtype CellKeyStore  = CellKeyStore { getCellKeyStore :: (S.Set FileKey)}
    deriving (Typeable,Show,Generic)

emptyCellKeyStore = CellKeyStore S.empty

$(deriveSafeCopy 0 'base ''CellKeyStore)

-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for 
--  dormant filenames

type TCellCore  k src dst tm stlive stdormant =  (CellCore  k src dst tm  stlive stdormant)


data AcidCell  k src dst tm stlive stdormant = AcidCell { 
      cellCore :: TCellCore  k src dst tm (AcidState stlive) stdormant 
    , cellKey  :: CellKey  k src dst tm stlive
      -- stdormant here because that is the actual acid state representation 
    } 
   deriving (Typeable,Generic)





-- |Cell Core interaction functions 
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Acidic Core


-- | Path manipulation happens at every atomic change to the Cell
-- These functions are made acidic 
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it

-- | DIG FileKey interface is where the acidFunctions live They are functions of fileKey without the conversions

deleteAcidCellPathFileKey :: FileKey -> Update CellKeyStore FileKey 
deleteAcidCellPathFileKey fk = do 
  (CellKeyStore { getCellKeyStore = hsSet}) <- get 
  (void $ put $ (CellKeyStore (S.delete  fk hsSet )))
  return fk


-- |Note... This insert is repsert functional
insertAcidCellPathFileKey :: FileKey ->  Update CellKeyStore FileKey
insertAcidCellPathFileKey fk =  do 
  (CellKeyStore { getCellKeyStore = hsSet}) <- get 
  (void $ put $ (CellKeyStore (S.insert  fk hsSet )))
  return fk



  

getAcidCellPathFileKey :: Query CellKeyStore ((S.Set FileKey))
getAcidCellPathFileKey = do
  (CellKeyStore { getCellKeyStore = hsSet}) <- ask
  case hsSet of 
    _
        |S.null hsSet -> return  S.empty
        | otherwise -> do 
               return  hsSet
  

$(makeAcidic ''CellKeyStore ['deleteAcidCellPathFileKey, 'insertAcidCellPathFileKey, 'getAcidCellPathFileKey])

-- | DIG interface


deleteAcidCellPath :: MonadIO m =>
                            CellKey k src dst tm st
                            -> AcidState (EventState DeleteAcidCellPathFileKey)
                            -> st
                            -> m (EventResult DeleteAcidCellPathFileKey)
deleteAcidCellPath ck  fAcid stTarget = do 
  update' fAcid (DeleteAcidCellPathFileKey (makeFileKey ck stTarget))


insertAcidCellPath :: MonadIO m =>
                            CellKey k src dst tm st
                            -> AcidState (EventState InsertAcidCellPathFileKey)
                            -> st
                            -> m (EventResult InsertAcidCellPathFileKey)
insertAcidCellPath ck fAcid stTarget =  do 
  update' fAcid (InsertAcidCellPathFileKey (makeFileKey ck stTarget))


-- getAcidCellPath :: MonadIO m =>
--                          CellCore
--                            t t1 t2 t3 t4 (AcidState (EventState GetAcidCellPathFileKey))
--                          -> m (EventResult GetAcidCellPathFileKey)

-- getAcidCellPath (CellCore _ fAcid) = do
--   query' fAcid GetAcidCellPathFileKey

-- | User Interface Defining Functions

-- | The 'st' in the type definition here is the AcidState that will be turned into a watched state

insertState :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic t) =>
                     CellKey k src dst tm st
                     -> t
                     -> AcidCell
                          k src dst tm t (AcidState (EventState InsertAcidCellPathFileKey))
                     -> st
                     -> IO (AcidState t)
insertState ck  initialTargetState (AcidCell (CellCore tlive fAcid) _)  st = do 
  let newStatePath = (codeCellKeyFilename ck).(getKey ck) $ st
  void $ insertAcidCellPath ck fAcid  st
  acidSt <- openLocalStateFrom (T.unpack newStatePath) initialTargetState 
  atomically (stmInsert acidSt)
  createCheckpoint fAcid 
  return acidSt 
   where 
     stmInsert st' = do 
       liveMap <- readTVar tlive        
       writeTVar tlive $ M.insert (getKey ck st) st' liveMap

               

deleteState :: (Ord k, Ord src, Ord dst, Ord tm) =>
                     CellKey k src dst tm st
                     -> AcidCell
                          k src dst tm t (AcidState (EventState DeleteAcidCellPathFileKey))
                     -> st
                     -> IO ()
deleteState ck (AcidCell (CellCore tlive fAcid) _) st = do 
  let targetStatePath = (codeCellKeyFilename ck).(getKey ck) $ st :: Text 
      targetFP = fromText targetStatePath ::FilePath     
  void $ atomically stmDelete
  void $ deleteAcidCellPath ck fAcid st
  createCheckpoint fAcid
  removeTree targetFP 
      where
        stmDelete = do 
          liveMap <- readTVar tlive
          writeTVar tlive $ M.delete (getKey ck st) liveMap


getState :: (Ord k, Ord src, Ord dst, Ord tm) =>
                  CellKey k src dst tm st
                  -> AcidCell k src dst tm t t1 -> st -> IO (Maybe (AcidState t))
getState ck (AcidCell (CellCore tlive _) _) st = do
   stmGetIO
      where
        stmGetIO = do 
          liveMap <- readTVarIO tlive
          return $ M.lookup (getKey ck st) liveMap 


stateFoldlWithKey :: t6   -> AcidCell t t1 t2 t3 t4 t5
                           -> (t6
                               -> DirectedKeyRaw t t1 t2 t3 -> AcidState t4 -> IO b -> IO b)
                           -> IO b
                           -> IO b
stateFoldlWithKey ck (AcidCell (CellCore tlive _) _) fldFcn seed = do 
  liveMap <- readTVarIO tlive 
  M.foldWithKey (fldFcn ck ) seed liveMap
  


createCellCheckPointAndClose :: SafeCopy st =>
                                      t
                                      -> AcidCell t1 t2 t3 t4 st t5
                                      -> IO (Map (DirectedKeyRaw t1 t2 t3 t4) ())
createCellCheckPointAndClose ck (AcidCell (CellCore tlive fAcid) _) = do 
  liveMap <- readTVarIO tlive 
  traverse createCheckpointAndClose liveMap


initializeAcidCell :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic stlive) =>
                            CellKey k src dst tm stlive
                            -> stlive
                            -> Text
                            -> IO (AcidCell k src dst tm stlive (AcidState CellKeyStore))
initializeAcidCell ck emptyTargetState root = do 
 oldWorkingDir <- getWorkingDirectory
 let acidRootPath = fromText root
     newWorkingDir = oldWorkingDir </> acidRootPath
 fAcidSt <- openLocalStateFrom (T.unpack root) emptyCellKeyStore 
 setWorkingDirectory newWorkingDir -- have to wait till it gets created by open local state
 fkSet   <-   query' fAcidSt (GetAcidCellPathFileKey)
 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet  
 stateMap <- foldlM foldMFcn  M.empty setEitherFileKeyRaw 
 tmap <- newTVarIO stateMap
 return $ AcidCell (CellCore tmap fAcidSt) ck
    where
     foldMFcn  cellMap (Left _)   = return cellMap 
     foldMFcn  cellMap (Right fkRaw) = do 
       st' <- openLocalStateFrom (T.unpack.(codeCellKeyFilename ck) $ fkRaw) emptyTargetState 
       return $ M.insert fkRaw st' cellMap 

  




  

-- | Exception and Error handling
-- type AEither a = Either AcidCellError a

data AcidCellError   = InsertFail    Text 
                     | DeleteFail    Text
                     | StateNotFound Text
