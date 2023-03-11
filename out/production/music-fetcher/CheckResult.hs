module CheckResult(checkResult) where

import Models
import FileInput (Title)
import Data.Char (toLower)

filterRecordings :: Title -> Maybe Recording -> MaybeRecording -> Maybe Recording
filterRecordings expectedTitle Nothing (MaybeRecording t (Just md) a album trackPos) | (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t md a album trackPos
filterRecordings expectedTitle (Just (Recording _ d1 a _ _)) (MaybeRecording t (Just d2) _ album trackPos) | d2 < d1 && (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t d2 a album trackPos
filterRecordings expectedTitle r (MaybeRecording t (Just _) _ _ _) | expectedTitle == t = r
filterRecordings _ r _ = r

checkResult :: Title -> PreliminaryRecordingsResponse -> Maybe Recording
checkResult t (PreliminaryRecordingsResponse _ rs) = foldl (filterRecordings t) Nothing rs