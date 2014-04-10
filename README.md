# acid-cell
Acid Cell takes a filename generating function and an Acid datatype
it then generates the machinery to handle the creation of multiple atomic values of this type according to the unique keys of each one.

This avoids the write locking problem in Acid systems by using two separated data ideas for each piece.

## Installation

TODO: Write installation instructions here

## Usage

### Inputs ...

AcidCells are designed to take a function to retrieve keys and a function to make those keys into filenames

The output of the functions should be unique *AcidCell* does not check this for you

``` haskell


-- given

-- The user supplies an atomic acid state variable

myAcidStateVariable = x :: SomeAcidState


-- define

myRootDir :: FilePath
myRootDir = "./stateSpace"


-- Fill this guy
data CellKey k h s t st = CellKey { getKey :: st -> (DirectedKeyRaw k h s t)
                                  , codeCellKeyFilename :: (DirectedKeyRaw k h s t) -> Text
                                  , decodeCellKeyFilename :: Text -> (DirectedKeyRaw k h s t)
                                  }
                    

$(createAcidCell `myRootDir ''SomeAcidState 'myCellKey)

```

### Creates ...

Here is what the template haskell call above generates.

``` haskell

-- data types for managing states in the cell both active and dormant
data CellCore k h s t stlive stdormant = CellCore { 
      ccLive :: (M.Map (DirectedKeyRaw k h s t) stlive )
      ccDormant :: (M.Map (DirectedKeyRaw k h s t) stdormant )
    }

type TCellCore k h s t stlive stdormant = TVar (CellCore k h s t (TVar stlive) (TVar stdormant))

-- Generate dig for CellCoreDormant
-- These are not directly availalbe to the user
insertCellSomeAcidPath :: CellCore -> SomeAcidState -> Update ...
deleteCellSomeAcidPath :: CellCore -> SomeAcidState -> Update ...
getCellSomeAcidPath    :: CellCore -> SomeAcidState -> Query ...   

-- DIG structure 

data AcidCell someacidstate = AcidCell {                                      
                                       , cellCore :: TCellCore ... 
                                       , cellKeys :: CellKey ...
                                       , cellRoot :: Text
                                      }


-- UI Functions
insertState :: AcidCell -> <SomeAcidState> -> IO (Either AcidCellError DirectedKeyRaw)
deleteState :: AcidCell -> DirectedKeyRaw -> IO Bool
getState      :: AcidCell -> DirectedKeyRaw -> IO (Either AcidCellError SomeAcidState)
queryCell   :: AcidCell -> (SomeAcidState -> a ) -> IO (Either AcidCellError (monoid a))

-- updateState
-- deleteWhere


type AcidCellSomeAcidState = AcidCell SomeAcidState


makeCellSomeAcidState :: IO AcidCellSomeAcidState

``` 

### Use

``` haskell


someStateManip :: IO ()
    acell <- makeCellSomeAcidState
    drId  <- insertState acell SomeAcidState
    rslt  <- queryCell acell allConsistentStates 
    ast   <- getState acell drId
    deleteState drId 
    

```

a *DirectedKeyRaw* is a key with a source and a destination.  The source and destination are very arbitrary but were
created to deal with database keys coming from multiple locations.  

