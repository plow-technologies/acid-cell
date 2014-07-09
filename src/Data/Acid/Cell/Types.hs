{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable,ScopedTypeVariables #-}

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
                            , updateState
                            , deleteState
                            , stateFoldlWithKey
                            , stateTraverseWithKey
                            , createCellCheckPointAndClose
                            , archiveAndHandle 
                            ) where


-- System 
import Filesystem.Path.CurrentOS hiding (root)
import Filesystem 

-- Controls
import Prelude (show, (++) )
import CorePrelude hiding (try,onException,catch)
import Control.Concurrent.STM
import Control.Monad.Reader ( ask )
import Control.Monad.State  
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
-- Typeclasses
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
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

import Data.Acid.Cell.Internal

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
      ccLive     :: ! (TVar (M.Map (DirectedKeyRaw  k src dst tm) tvlive ))
      ,ccDormant :: !(TVar stdormant)
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
      cellCore :: !(TCellCore  k src dst tm (AcidState stlive) stdormant )
    , cellKey  :: !(CellKey  k src dst tm stlive)
    , cellParentFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir
    , cellRootFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir/openLocalStateFromdir
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

-- | Warning, inserting a state that is already inserted throws an exception 

insertState :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic t,IsAcidic st) =>
                     CellKey k src dst tm st
                     -> t
                     -> AcidCell
                          k src dst tm st (AcidState (EventState InsertAcidCellPathFileKey))
                     -> st
                     -> IO (AcidState st)
insertState ck  initialTargetState (AcidCell (CellCore tlive tvarFAcid) _ pdir rdir)  st = do 
  let newStatePath = (codeCellKeyFilename ck).(getKey ck) $ st
  fullStatePath <- makeWorkingStatePath pdir rdir newStatePath
  fAcid <- readTVarIO tvarFAcid
  void $ insertAcidCellPath ck fAcid  st  
  eacidSt <- try (openLocalStateFrom (encodeString fullStatePath) st )  
  either (\(e::IOException)  -> lockOrThere e newStatePath) (\acidSt -> do
                                atomically (stmInsert acidSt)                                
                                createCheckpoint fAcid 
                                atomically $ writeTVar tvarFAcid fAcid
                                return acidSt ) eacidSt
   where 
     stmInsert st' = do 
       liveMap <- readTVar tlive        
       writeTVar tlive $ M.insert (getKey ck st) st' liveMap
     lockOrThere e fp = fail (("insertState Failed to insert! State locked or already exists" ++ (show e)))


-- | Generate the state path from the full path of the working directory... pdir 
--                                the path piece of the filekeystate... rdir 
--                                the path piece of the cell        ... nsp
                                 
makeWorkingStatePath pdir rdir nsp = do 
    void $ when (nsp == "") (fail "--> Cell key led to empty state path")
    return $ pdir </> rdir </> (fromText nsp)

updateState ck  initialTargetState (AcidCell (CellCore tlive tvarFAcid) _ pdir rdir )  acidSt st = do
--  let statePath = fromText.(codeCellKeyFilename ck).(getKey ck) $ st
--  createCheckpoint acidSt 
  atomically $ stmInsert acidSt
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
deleteState ck (AcidCell (CellCore tlive tvarFAcid) _ pdir rdir) st = do 
  let targetStatePath = (codeCellKeyFilename ck).(getKey ck) $ st 
      targetFP = fromText targetStatePath ::FilePath       
  void $ atomically stmDelete
  fAcid <- readTVarIO tvarFAcid
  void $ deleteAcidCellPath ck fAcid st  
  createCheckpoint fAcid
  atomically $ writeTVar tvarFAcid fAcid
  np <- (makeWorkingStatePath pdir rdir targetStatePath)
  removeTree np
      where
        stmDelete = do 
          liveMap <- readTVar tlive
          writeTVar tlive $ M.delete (getKey ck st) liveMap


getState :: (Ord k, Ord src, Ord dst, Ord tm) =>
                  CellKey k src dst tm st
                  -> AcidCell k src dst tm t t1 -> st -> IO (Maybe (AcidState t))
getState ck (AcidCell (CellCore tlive _) _ _ _) st = do
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

stateFoldlWithKey ck (AcidCell (CellCore tlive _) _ _ _) fldFcn seed = do 
  liveMap <- readTVarIO tlive 
  M.foldWithKey (fldFcn ck ) seed liveMap


stateTraverseWithKey :: forall t t1 t2 t3 t4 t5 t6 b.
                         t6
                         -> AcidCell t t1 t2 t3 t4 t5
                         -> (t6 -> DirectedKeyRaw t t1 t2 t3 -> AcidState t4 -> IO b)
                         -> IO (Map (DirectedKeyRaw t t1 t2 t3) b)

stateTraverseWithKey ck (AcidCell (CellCore tlive _) _ _ _) tvFcn  = do 
  liveMap <- readTVarIO tlive 
  M.traverseWithKey tvFcnWrp  liveMap
      where
        tvFcnWrp k a = do
          ( tvFcn ck k a) -- (\e -> either (\e' -> print e') (\_ -> return () ) e )
          
          
createCellCheckPointAndClose :: forall t t1 t2 t3 t4 st st1.
                                (SafeCopy st1, Typeable st1) =>
                                t -> AcidCell t1 t2 t3 t4 st (AcidState st1) -> IO ()

createCellCheckPointAndClose _ (AcidCell (CellCore tlive tvarFAcid) _ pdir rdir ) = do 
  liveMap <- readTVarIO tlive 
  void $ traverse (\st -> (onException (closeAcidState st) (print "error closing state") )) liveMap
  fAcid <- readTVarIO tvarFAcid
  void $ createCheckpointAndClose fAcid





archiveAndHandle :: CellKey k src dst tm st
                          -> AcidCell k src dst tm st1 (AcidState st2)
                          -> (FilePath -> AcidState st1 -> IO (AcidState st1))
                          -> IO (Map (DirectedKeyRaw k src dst tm) (AcidState st1))
archiveAndHandle ck (AcidCell (CellCore tlive tvarFAcid) _ pDir rDir) entryGC = do 
  liveMap <- readTVarIO tlive 
  rslt <-  M.traverseWithKey (\dkr st -> catch (gcWrapper dkr st) (\(SomeException e) -> print "top level exception" >> print e >> return st) )  liveMap  
  fAcid <- readTVarIO tvarFAcid
  catch (createArchive fAcid) (\(SomeException e) -> print "at archive point" >> print e)
  atomically $ writeTVar tvarFAcid fAcid
  atomically $ writeTVar tlive rslt
  return rslt
    where 
      targetStatePath dkr = fromText.(codeCellKeyFilename ck) $ dkr :: FilePath
      gcWrapper dkr st = do
        let stateLocalFP = (targetStatePath dkr)
        void $ catch (createArchive st ) (\(SomeException e) -> print "at local archive point" >> print e)

        rslt <- entryGC stateLocalFP st
        return rslt
--      targetFP =  targetStatePath ::FilePath     
      

initializeAcidCell :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic stlive) =>
                            CellKey k src dst tm stlive
                            -> stlive
                            -> Text                            -> IO (AcidCell k src dst tm stlive (AcidState CellKeyStore))
initializeAcidCell ck emptyTargetState root = do 
-- print "get WD"
 parentWorkingDir <- getWorkingDirectory
 let acidRootPath = fromText root
     newWorkingDir = acidRootPath
     fpr           = (parentWorkingDir </> acidRootPath)
-- print "get fAcidSt"
 fAcidSt <- openLocalStateFrom (encodeString fpr ) emptyCellKeyStore 
-- print "get fkSet"
 fkSet   <-   query' fAcidSt (GetAcidCellPathFileKey)
-- print "get unmakeThing"
 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet  
--  print "get fkSet"
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw)
 aStateList <- traverse (traverseAndWait fpr) groupedList
 -- stateMap <- foldlM (foldMFcn fpr) M.empty setEitherFileKeyRaw
 -- stateList <- traverse (traverseLFcn fpr) setEitherFileKeyRaw
--  print "All threads processed. Creating map"
 let stateMap = M.fromList (rights . rights $ (Data.Foldable.concat aStateList))
--  print "get stateMap"
 tmap <- newTVarIO stateMap
--  print "get tvarFAcid"
 tvarFAcid <- newTVarIO fAcidSt
--  print "get acidcell"
 return $ AcidCell (CellCore tmap tvarFAcid) ck parentWorkingDir newWorkingDir
    where
      --foldMFcn r cellMap (Left e)   = print "cellmap err" >> print e >> return cellMap 
      --foldMFcn r cellMap (Right fkRaw) = do 
      --  let fpKey = r </> (fromText.(codeCellKeyFilename ck) $ fkRaw) 
      --  print fpKey
      --  est' <- openCKSt fpKey emptyTargetState
      --  either (\_-> return cellMap ) (\st' -> return $ M.insert fkRaw st' cellMap ) est'
      traverseAndWait f l = do
        aRes <- traverse (traverseLFcn f) l
        traverse waitCatch aRes
      traverseLFcn  r fkRaw = (async $ traverseLFcn' r fkRaw)
      traverseLFcn' r fkRaw = do 
        let fpKey = r </> (fromText . (codeCellKeyFilename ck) $ fkRaw) 
        est' <- openCKSt fpKey emptyTargetState
        print $ "opened: " ++ (show fpKey)    
        return $ fmap (\st' -> (fkRaw, st')) est'       
        -- either (\_-> return cellMap ) (\st' -> return $ M.insert fkRaw st' cellMap ) est'


openCKSt :: IsAcidic st =>
             FilePath -> st -> IO (Either SomeException (AcidState st))
openCKSt fpKey emptyTargetState = try $ openLocalStateFrom (encodeString fpKey) emptyTargetState 


  

-- | Exception and Error handling
-- type AEither a = Either AcidCellError a

data AcidCellError   = InsertFail    !Text 
                     | DeleteFail    !Text
                     | StateNotFound !Text
