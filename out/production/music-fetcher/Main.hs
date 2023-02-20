{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Data.Text (Text)

data QueryResponse = QueryResponse {
  artists :: [Artist]
} deriving (Generic, Show)

data Artist = Artist {
    name :: Text
  , id :: Text
} deriving (Generic, Show)

instance FromJSON Artist where

instance FromJSON QueryResponse where

query = "query" =: ("name:Pink Floyd" :: Text)

headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
main :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
main = runReq defaultHttpConfig $ do
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      GET -- method
      (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
      NoReqBody
      jsonResponse -- specify how to interpret response
      (mappend headers query) -- query params, headers, explicit port number, etc.
  liftIO $ print (responseBody r :: QueryResponse)
