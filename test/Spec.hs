{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import PreprocessSpec
import PostprocessSpec

tests :: Test
tests = TestList [PreprocessSpec.spec, PostprocessSpec.spec]

main :: IO Counts
main = runTestTT tests
