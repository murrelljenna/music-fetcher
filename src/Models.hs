{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Models(QueryResponse, Artist(..), refineDiscography, recordingArtist, firstArtist, FinalResult(..), RecordingsQueryResponse, PreliminaryRecordingsResponse(..), artistId, MaybeRecording(..), Recording(..)) where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import FileInput
import Data.Char (toLower)
import Data.Aeson.Types (Parser)

data QueryResponse = QueryResponse {
  artists :: [Artist]
} deriving (Generic, Show)

data RecordingsQueryResponse = RecordingsQueryResponse {
  count :: Int
  , recordings :: [Recording]
} deriving (Generic, Show)

data Recording = Recording {
  title :: String
  , date :: String
  , artist :: Artist
  , album :: String
  , position :: String
} deriving (Generic, Show, Eq)

data PreliminaryRecordingsResponse = PreliminaryRecordingsResponse {
  count :: Int
  , maybeRecordings :: [MaybeRecording]
  } deriving (Generic, Show)

data MaybeRecording = MaybeRecording {
  title :: String
  , maybeDate :: Maybe String
  , artist :: Artist
  , album :: String
  , position :: String
} deriving (Generic, Show)

recordingArtist :: Recording -> Artist
recordingArtist (Recording _ _ a _ _) = a

data Artist = Artist {
    name :: String
  , id :: String
} deriving (Generic, Show, Eq)

data FinalResult = FinalResult {
    artist :: Artist
    , recording :: Recording
} deriving (Show)

artistId :: Artist -> String
artistId a = id a

firstArtist :: QueryResponse -> Artist
firstArtist res = head $ artists res

instance FromJSON Artist where

instance FromJSON QueryResponse where
instance FromJSON Recording where

instance FromJSON RecordingsQueryResponse where

instance FromJSON PreliminaryRecordingsResponse where
      parseJSON = withObject "PreliminaryRecordingsResponse" $ \v -> PreliminaryRecordingsResponse
          <$> v .: "count"
          <*> v .: "recordings"
instance FromJSON MaybeRecording where
      parseJSON = withObject "MaybeRecording" $ \v -> MaybeRecording
          <$> v .: "title"
          <*> v .:? "first-release-date"
          <*> (do
            artistCredit <- head <$> (v .: "artist-credit" :: Parser [Object])
            let artistParser = (artistCredit .: "artist" :: Parser Artist)
            artistParser)
          <*> (do
            release <- head <$> (v .: "releases" :: Parser [Object])
            let albumParser = (release .: "title" :: Parser String)
            albumParser)
          <*> (do
            release <- head <$> (v .: "releases" :: Parser [Object])
            media <- head <$> (release .: "media" :: Parser [Object])
            track <- head <$> (media .: "track" :: Parser [Object])
            let positionParser = (track .: "number" :: Parser String)
            positionParser)

-- Sus out which discography entry to use
-- Must have a first-release-date, use earliest first release date

filterRecordings :: Title -> Maybe Recording -> MaybeRecording -> Maybe Recording
filterRecordings expectedTitle Nothing (MaybeRecording t (Just md) a album trackPos) | (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t md a album trackPos
filterRecordings expectedTitle (Just (Recording _ d1 a _ _)) (MaybeRecording t (Just d2) _ album trackPos) | d2 < d1 && (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t d2 a album trackPos
filterRecordings expectedTitle r (MaybeRecording t (Just _) _ _ _) | expectedTitle == t = r
filterRecordings _ r _ = r

refineDiscography :: Title -> PreliminaryRecordingsResponse -> Maybe Recording
refineDiscography t (PreliminaryRecordingsResponse _ rs) = foldl (filterRecordings t) Nothing rs
