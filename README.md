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

myRootDir :: Text
myRootDir = "./stateSpace"

mycellKey = CellKey { getKey = foo       --  foo :: SomeAcidState -> DirectedKeyRaw
                    , makeFilename = bar --  bar :: DirectedKeyRaw -> ValidFileText
                    }

$(createAcidCell `myRootDir ''SomeAcidState 'myCellKey)

```

### Creates ...

Here is what the template haskell call above generates.

``` haskell

-- data types for managing states in the cell both active and dormant

type CellCoreLive  = Map DirectedKeyRaw (MVar SomeAcidState) -- Live AcidStates can be looked up
type CellCoreDormant = Map DirectedKeyRaw FileName -- File Locations of the states


-- Generate dig for CellCoreDormant
insertCellSomeAcidState :: CellCoreDormant -> DirectedKeyRaw -> Update ...
deleteCellSomeAcidState :: CellCoreDormant -> DirectedKeyRaw -> Update ...
getCellSomeAcidState    :: CellCoreDormant -> DirectedKeyRaw -> Query ...   

-- DIG structure 

data AcidCell someacidstate = AcidCell {
                                         insertState :: <SomeAcidState> -> IO (Either AcidCellError DirectedKeyRaw)
                                       , deleteState :: DirectedKeyRaw -> IO Bool
                                       , getState    :: DirectedKeyRaw -> IO (Either AcidCellError SomeAcidState)                                         
                                       , queryCell   :: (SomeAcidState -> a ) -> IO (Either AcidCellError (monoid a))
                                       , cellCoreLive    :: (MVar CellCoreLive) -- the live, MVar version of the cell structure
                                       , cellCoreDormant :: (AcidState CellCoreDormant) -- the storable Acid version 

                                      }
-- updateState
-- deleteWhere


type AcidCellSomeAcidState = AcidCell SomeAcidState


makeCellSomeAcidState :: IO AcidCellSomeAcidState

``` 

### Use

``` haskell


someStateManip :: IO ()
    acell <- makeCellSomeAcidState
    drId  <- insertState SomeAcidState
    ast   <- getState drId
    deleteState drId 
    

```

a *DirectedKeyRaw* is a key with a source and a destination.  The source and destination are very arbitrary but were
created to deal with database keys coming from multiple locations.  

