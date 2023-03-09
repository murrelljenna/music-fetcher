{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Models(QueryResponse, Artist, refineDiscography, firstArtist, FinalResult(..), RecordingsQueryResponse, PreliminaryRecordingsResponse(..), artistId, MaybeRecording(..), Recording(..)) where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import Input
import Data.Char (toLower)

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
} deriving (Generic, Show, Eq)

data PreliminaryRecordingsResponse = PreliminaryRecordingsResponse {
  count :: Int
  , maybeRecordings :: [MaybeRecording]
  } deriving (Generic, Show)

data MaybeRecording = MaybeRecording {
  title :: String
  , maybeDate :: Maybe String
} deriving (Generic, Show)

data Artist = Artist {
    name :: String
  , id :: String
} deriving (Generic, Show)

data FinalResult = FinalResult {
    artist :: Maybe Artist
    , recording :: Recording
} deriving (Show)

artistId :: Artist -> String
artistId artist = id artist

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


-- Sus out which discography entry to use
-- Must have a first-release-date, use earliest first release date

filterRecordings :: Title -> Maybe Recording -> MaybeRecording -> Maybe Recording
filterRecordings expectedTitle Nothing (MaybeRecording t (Just md)) | (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t md
filterRecordings expectedTitle (Just (Recording _ d1)) (MaybeRecording t (Just d2)) | d2 < d1 && (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t d2
filterRecordings expectedTitle r (MaybeRecording t (Just _)) | expectedTitle == t = r
filterRecordings _ r _ = r

refineDiscography :: Title -> PreliminaryRecordingsResponse -> Maybe Recording
refineDiscography title (PreliminaryRecordingsResponse c rs) = foldl (filterRecordings title) Nothing rs
