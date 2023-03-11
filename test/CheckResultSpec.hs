module CheckResultSpec(spec) where

import Test.HUnit
import FileInput
import Models
import CheckResult

spec :: Test
spec = TestList [oneRecordingSpec, noDateSpec, noRecordingSpec]

oneRecordingSpec :: Test
oneRecordingSpec = TestCase $ assertEqual
  "Accepts recording where only one recording with date is present"
  oneRecordingExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> oneRecordingExpectedInputs)

oneRecordingFixture :: [((Title, PreliminaryRecordingsResponse), Maybe Recording)]
oneRecordingFixture = [
  (("Grey Day"
  , PreliminaryRecordingsResponse 1 [(MaybeRecording "Grey Day" (Just "2008-11-27") (Artist "johny exampleseed" "blargh") "idunno" "hey")])
  , Just (Recording "Grey Day" "2008-11-27" (Artist "johny exampleseed" "blargh") "idunno" "hey"))
  ]

oneRecordingExpectedInputs :: [(Title, PreliminaryRecordingsResponse)]
oneRecordingExpectedInputs = fst <$> oneRecordingFixture

oneRecordingExpectedResults :: [Maybe Recording]
oneRecordingExpectedResults = snd <$> oneRecordingFixture

--

noRecordingSpec :: Test
noRecordingSpec = TestCase $ assertEqual
  "Returns nothing when no recordings found"
  noRecordingExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> noRecordingExpectedInputs)

noFixture :: [((Title, PreliminaryRecordingsResponse), Maybe Recording)]
noFixture = [
              (("Grey Day"
              , PreliminaryRecordingsResponse 1 [])
              , Nothing)
              ]

noRecordingExpectedInputs :: [(Title, PreliminaryRecordingsResponse)]
noRecordingExpectedInputs = fst <$> noFixture

noRecordingExpectedResults :: [Maybe Recording]
noRecordingExpectedResults = snd <$> noFixture

--

noDateSpec :: Test
noDateSpec = TestCase $ assertEqual
  "Returns nothing when no recordings with release date are found"
  noDateExpectedResults
  ((\tuple -> case tuple of (t, rs) -> checkResult t rs) <$> noDateExpectedInputs)

noDateFixture :: [((Title, PreliminaryRecordingsResponse), Maybe Recording)]
noDateFixture = [
              (("Grey Day"
              , PreliminaryRecordingsResponse 1 [(MaybeRecording "Grey Day" Nothing (Artist "johny exampleseed" "blargh") "idunno" "hey")])
              , Nothing)
              ]

noDateExpectedInputs :: [(Title, PreliminaryRecordingsResponse)]
noDateExpectedInputs = fst <$> noDateFixture

noDateExpectedResults :: [Maybe Recording]
noDateExpectedResults = snd <$> noDateFixture

