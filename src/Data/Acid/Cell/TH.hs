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

-- Controls
import Control.Applicative
import Data.Traversable

-- Component Libraries
import Data.Acid.Cell.TH.StateMakers


-- | use this function to make your acid Cell 
-- > $(makeAcidCell 'yourCellKey 'emptyState 'SomeTarget)
makeAcidCell :: Name -> Name -> Name ->  Q [Dec]
makeAcidCell ckN initN stN = do 
  Data.Traversable.sequence $ allStateMakers <*> [ckN] <*> [initN] <*> [stN]

