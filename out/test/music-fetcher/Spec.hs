{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Input
import Models(PreliminaryRecordingsResponse(..), MaybeRecording(..), Recording(..), refineDiscography)

parseCandidatesTest :: Test
parseCandidatesTest = TestCase $ assertEqual "" ["Zoot Woman", "Grey Day"] (parseCandidatesFromTitle "Zoot Woman - Grey Day")

responseFixture :: PreliminaryRecordingsResponse
responseFixture = PreliminaryRecordingsResponse 1 [MaybeRecording "My Friend Dario" (Just "2005-05-21")]

refineDiscographyTest :: Test
refineDiscographyTest = TestCase $ assertEqual "" (Just (Recording "My Friend Dario" "2005-05-21")) (refineDiscography responseFixture)


tests :: Test
tests = TestList [parseCandidatesTest, refineDiscographyTest]

main :: IO Counts
main = runTestTT tests
