module CheckResult(checkResult) where

import Models
import FileInput (Title)
import Data.Char (toLower)

filterRecordings :: Title -> Maybe Recording -> Recording -> Maybe Recording
filterRecordings expectedTitle Nothing (Recording t md a album trackPos) | (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t md a album trackPos
filterRecordings expectedTitle (Just (Recording _ d1 a _ _)) (Recording t d2 _ album trackPos) | d2 < d1 && (toLower <$> expectedTitle) == (toLower <$> t) = Just $ Recording t d2 a album trackPos
filterRecordings expectedTitle r (Recording t _ _ _ _) | expectedTitle == t = r
filterRecordings _ r _ = r

checkResult :: Title -> RecordingsResponse -> Maybe Recording
checkResult t (PreliminaryRecordingsResponse _ rs) = foldl (filterRecordings t) Nothing rs