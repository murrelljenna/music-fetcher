{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import CandidateSpec
import CheckResultSpec

tests :: Test
tests = TestList [CandidateSpec.spec, CheckResultSpec.spec]

main :: IO Counts
main = runTestTT tests
