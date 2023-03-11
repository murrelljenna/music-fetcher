{-# LANGUAGE OverloadedStrings #-}

module Api(fetchArtists, fetchArtist, fetchArtistDiscography, fetchDiscography) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Models
import Data.Aeson.Types (Object)

fetchArtists :: [String] -> Req [Artist]
fetchArtists names = sequence $ fetchArtist <$> names

fetchArtist :: String -> Req Artist
fetchArtist name = do
                 let query = "query" =: ("name:" <> name :: String)
                 let headers = mappend mempty header "User-Agent" "music-fetcher/0.1.0 ( https://github.com/murrelljenna/music-fetcher )"
                 r <-
                   req
                     GET -- method
                     (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
                     NoReqBody
                     jsonResponse -- specify how to interpret response
                     (mappend headers query) -- query params, headers, explicit port number, etc.
                 liftIO $ return $ firstArtist (responseBody r :: QueryResponse)

fetchArtistDiscography :: (Artist, String) -> Req PreliminaryRecordingsResponse
fetchArtistDiscography (artist, title) = do
                let query = "query" =: ("\"" <> title <> "\" AND arid:" <> (artistId artist) :: String)
                let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
                r <-
                  req
                    GET -- method
                    (https "musicbrainz.org" /: "ws" /: "2" /: "recording") -- safe by construction URL
                    NoReqBody
                    jsonResponse -- specify how to interpret response
                    (mappend headers query) -- query params, headers, explicit port number, etc.
                liftIO $ return (responseBody r :: PreliminaryRecordingsResponse)

fetchDiscography :: String -> Req PreliminaryRecordingsResponse
fetchDiscography title = do
               let query = "query" =: ("\"" <> title :: String)
               let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
               r <-
                 req
                   GET -- method
                   (https "musicbrainz.org" /: "ws" /: "2" /: "recording") -- safe by construction URL
                   NoReqBody
                   jsonResponse -- specify how to interpret response
                   (mappend headers query) -- query params, headers, explicit port number, etc.
               liftIO $ return (responseBody r :: PreliminaryRecordingsResponse)