{-# LANGUAGE DeriveGeneric #-}

module Models(QueryResponse, Artist) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data QueryResponse = QueryResponse {
  artists :: [Artist]
} deriving (Generic, Show)

data DiscQueryResponse = DiscQueryResponse {
  
} deriving (Generic, Show)

data Artist = Artist {
    name :: Text
  , id :: Text
} deriving (Generic, Show)

instance FromJSON Artist where

instance FromJSON QueryResponse where