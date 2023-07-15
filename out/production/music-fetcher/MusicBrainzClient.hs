{-# LANGUAGE OverloadedStrings #-}

module MusicBrainzClient(fetchArtists, fetchArtist, fetchArtistDiscography, fetchDiscography) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Models

fetchArtists :: [String] -> Req [Artist]
fetchArtists names = sequence $ fetchArtist <$> names

fetchArtist :: String -> Req Artist
fetchArtist artistName = do
                 let query = "query" =: ("name:" <> artistName :: String)
                 let headers = mappend mempty header "User-Agent" "music-fetcher/0.1.0 ( https://github.com/murrelljenna/music-fetcher )"
                 r <-
                   req
                     GET -- method
                     (https "musicbrainz.org" /: "ws" /: "2" /: "artist") -- safe by construction URL
                     NoReqBody
                     jsonResponse -- specify how to interpret response
                     (mappend headers query) -- query params, headers, explicit port number, etc.
                 liftIO $ return $ firstArtist (responseBody r :: QueryResponse)

fetchArtistDiscography :: (Artist, String) -> Req RecordingsResponse
fetchArtistDiscography (a, t) = do
                let query = "query" =: ("\"" <> t <> "\" AND arid:" <> (artistId a) :: String)
                let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
                r <-
                  req
                    GET -- method
                    (https "musicbrainz.org" /: "ws" /: "2" /: "recording") -- safe by construction URL
                    NoReqBody
                    jsonResponse -- specify how to interpret response
                    (mappend headers query) -- query params, headers, explicit port number, etc.
                liftIO $ return (responseBody r :: RecordingsResponse)

fetchDiscography :: String -> Req RecordingsResponse
fetchDiscography t = do
               let query = "query" =: ("\"" <> t :: String)
               let headers = mappend mempty header "User-Agent" "MusicBrainz API / Rate Limiting - MusicBrainz"
               r <-
                 req
                   GET -- method
                   (https "musicbrainz.org" /: "ws" /: "2" /: "recording") -- safe by construction URL
                   NoReqBody
                   jsonResponse -- specify how to interpret response
                   (mappend headers query) -- query params, headers, explicit port number, etc.
               liftIO $ return (responseBody r :: RecordingsResponse)