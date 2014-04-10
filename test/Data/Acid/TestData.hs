
module Data.Acid.Cell.TestData where 


import CorePrelude
import Data.Acid 


import qualified Data.Set as S
import DirectedKeys



data KeyedTestSet k a = KeyedTestSet { 
        testSetKey :: k
      , testSetData :: a
      }deriving (Show,Eq,Ord)
