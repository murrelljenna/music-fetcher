{-# LANGUAGE OverloadedStrings #-}

module Api(fetchArtists) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Models
import Data.Text (Text)

query = "query" =: ("name:Pink Floyd" :: Text)

headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"

fetchArtists :: Req QueryResponse
fetchArtists = do
                 r <-
                   req
                     GET -- method
                     (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
                     NoReqBody
                     jsonResponse -- specify how to interpret response
                     (mappend headers query) -- query params, headers, explicit port number, etc.
                 liftIO $ return (responseBody r :: QueryResponse)