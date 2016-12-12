-- @PersistentFields.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module PersistentFields where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics

data CategoriaTipo = Ansiedade | Desanimo | Fobia |
                    Perda | Vicio | Tristeza deriving (Show,Read,Eq,Generic)
derivePersistField "CategoriaTipo"


instance FromJSON CategoriaTipo
instance ToJSON CategoriaTipo
