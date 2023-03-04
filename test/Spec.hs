{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import InputSpec
import Models(PreliminaryRecordingsResponse(..), MaybeRecording(..), Recording(..), refineDiscography)

responseFixture :: PreliminaryRecordingsResponse
responseFixture = PreliminaryRecordingsResponse 1 [MaybeRecording "My Friend Dario" (Just "2005-05-21")]

refineDiscographyTest :: Test
refineDiscographyTest = TestCase $ assertEqual "" (Just (Recording "My Friend Dario" "2005-05-21")) (refineDiscography responseFixture)

tests :: Test
tests = TestList [InputSpec.spec, refineDiscographyTest]

main :: IO Counts
main = runTestTT tests
