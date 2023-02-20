{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Data.Text (Text)
import Api

data QueryResponse = QueryResponse {
  artists :: [Artist]
} deriving (Generic, Show)

data Artist = Artist {
    name :: Text
  , id :: Text
} deriving (Generic, Show)

instance FromJSON Artist where

instance FromJSON QueryResponse where

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- fetchArtists
  liftIO $ print r
