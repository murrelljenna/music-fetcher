{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import CandidateSpec
import Models(PreliminaryRecordingsResponse(..), MaybeRecording(..), Recording(..), refineDiscography, Artist(..))

responseFixture :: PreliminaryRecordingsResponse
responseFixture = PreliminaryRecordingsResponse 1 [MaybeRecording "My Friend Dario" (Just "2005-05-21") (Artist "" "")]

refineDiscographyTest :: Test
refineDiscographyTest = TestCase $ assertEqual "" (Just (Recording "My Friend Dario" "2005-05-21" (Artist "" ""))) (refineDiscography "My Friend Dario" responseFixture)

tests :: Test
tests = TestList [CandidateSpec.spec, refineDiscographyTest]

main :: IO Counts
main = runTestTT tests
