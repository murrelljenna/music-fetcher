module CheckResultSpec(spec) where

import Test.HUnit
import FileInput
import Models
import CheckResult

spec :: Test
spec = TestList [oneRecordingSpec, noRecordingSpec]

oneRecordingSpec :: Test
oneRecordingSpec = TestCase $ assertEqual
  "Accepts recording where only one recording with date is present"
  oneRecordingExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> oneRecordingExpectedInputs)

oneRecordingFixture :: [((Title, RecordingsResponse), Maybe Recording)]
oneRecordingFixture = [
  (("Grey Day"
  , RecordingsResponse 1 [(Recording "Grey Day" "2008-11-27" (Artist "johny exampleseed" "blargh") "idunno" "hey")])
  , Just (Recording "Grey Day" "2008-11-27" (Artist "johny exampleseed" "blargh") "idunno" "hey"))
  ]

oneRecordingExpectedInputs :: [(Title, RecordingsResponse)]
oneRecordingExpectedInputs = fst <$> oneRecordingFixture

oneRecordingExpectedResults :: [Maybe Recording]
oneRecordingExpectedResults = snd <$> oneRecordingFixture

--

noRecordingSpec :: Test
noRecordingSpec = TestCase $ assertEqual
  "Returns nothing when no recordings found"
  noRecordingExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> noRecordingExpectedInputs)

noFixture :: [((Title, RecordingsResponse), Maybe Recording)]
noFixture = [
              (("Grey Day"
              , RecordingsResponse 1 [])
              , Nothing)
              ]

noRecordingExpectedInputs :: [(Title, RecordingsResponse)]
noRecordingExpectedInputs = fst <$> noFixture

noRecordingExpectedResults :: [Maybe Recording]
noRecordingExpectedResults = snd <$> noFixture

--

noDateSpec :: Test
noDateSpec = TestCase $ assertEqual
  "Returns nothing when no recordings with release date are found"
  noDateExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> noDateExpectedInputs)

noDateFixture :: [((Title, RecordingsResponse), Maybe Recording)]
noDateFixture = [
              (("Grey Day"
              , RecordingsResponse 1 [(Recording "Grey Day" "05-02-2021" (Artist "johny exampleseed" "blargh") "idunno" "hey")])
              , Nothing)
              ]

noDateExpectedInputs :: [(Title, RecordingsResponse)]
noDateExpectedInputs = fst <$> noDateFixture

noDateExpectedResults :: [Maybe Recording]
noDateExpectedResults = snd <$> noDateFixture

