{-# LANGUAGE OverloadedStrings #-}

module Api(fetchArtists) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Models
import Data.Text (Text)

fetchArtists :: Text -> Req QueryResponse
fetchArtists name = do
                 let query = "query" =: ("name:" <> name :: Text)
                 let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
                 r <-
                   req
                     GET -- method
                     (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
                     NoReqBody
                     jsonResponse -- specify how to interpret response
                     (mappend headers query) -- query params, headers, explicit port number, etc.
                 liftIO $ return (responseBody r :: QueryResponse)
                
  