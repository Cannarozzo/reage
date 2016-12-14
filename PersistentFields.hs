-- @PersistentFields.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module PersistentFields where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics
import Data.Text

data CategoriaTipo = Ansiedade | Desanimo | Fobia |
                    Perda | Vicio | Tristeza deriving (Show,Read,Eq,Generic)
derivePersistField "CategoriaTipo"


instance FromJSON CategoriaTipo where
    

instance ToJSON CategoriaTipo where
    toJSON Ansiedade    =  object [ "Categoria" .= (pack.show) Ansiedade]
    toJSON Desanimo     =  object [ "Categoria" .= (pack.show) Desanimo ]
    toJSON Fobia        =  object [ "Categoria" .= (pack.show) Fobia    ]
    toJSON Perda        =  object [ "Categoria" .= (pack.show) Perda    ]
    toJSON Vicio        =  object [ "Categoria" .= (pack.show) Vicio    ]
    toJSON Tristeza     =  object [ "Categoria" .= (pack.show) Tristeza ]


{-
data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]


-}
{-

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    text Text
    done Bool
|]

instance ToJSON (Entity Todo) where
    toJSON (Entity tid (Todo text done)) = object
        [ "id" .= tid
        , "text" .= text
        , "done" .= done
        ]
instance FromJSON Todo where
    parseJSON (Object o) = Todo
        <$> o .: "text"
        <*> o .: "done"
    parseJSON _ = fail "Invalid todo"

-}