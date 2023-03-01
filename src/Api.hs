{-# LANGUAGE OverloadedStrings #-}

module Api(fetchArtists, fetchArtistDiscography) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Models
import Data.Text (Text)
import Data.Aeson.Types (Object)

fetchArtists :: Text -> Req QueryResponse
fetchArtists name = do
                 let query = "query" =: ("name:" <> name <> "&inc=recordings" :: Text)
                 let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
                 r <-
                   req
                     GET -- method
                     (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
                     NoReqBody
                     jsonResponse -- specify how to interpret response
                     (mappend headers query) -- query params, headers, explicit port number, etc.
                 liftIO $ return (responseBody r :: QueryResponse)

fetchArtistDiscography :: Artist -> Req PreliminaryRecordingsResponse
fetchArtistDiscography artist = do
                let query = "query" =: ("Wish you were here AND arid:" <> (artistId artist) :: Text)
                let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
                r <-
                  req
                    GET -- method
                    (https "musicbrainz.org" /: "ws" /: "2" /: "recording") -- safe by construction URL
                    NoReqBody
                    jsonResponse -- specify how to interpret response
                    (mappend headers query) -- query params, headers, explicit port number, etc.
                liftIO $ return (responseBody r :: PreliminaryRecordingsResponse)

