{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Models(QueryResponse, Artist, refineDiscography, firstArtist, RecordingsQueryResponse, PreliminaryRecordingsResponse(..), artistId, MaybeRecording(..), Recording(..)) where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson

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

filterRecordings :: Maybe Recording -> MaybeRecording -> Maybe Recording
filterRecordings Nothing (MaybeRecording t (Just md)) = Just $ Recording t md
filterRecordings (Just (Recording _ d1)) (MaybeRecording t (Just d2)) | d2 < d1 = Just $ Recording t d2
filterRecordings r (MaybeRecording _ (Just _)) = r
filterRecordings r _ = r

refineDiscography :: PreliminaryRecordingsResponse -> Maybe Recording
refineDiscography (PreliminaryRecordingsResponse c rs) = foldl filterRecordings Nothing rs
