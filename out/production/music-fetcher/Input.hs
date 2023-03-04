{-# LANGUAGE OverloadedStrings #-}

module Input(parseCandidatesFromTitle, fetchFilenames) where

import Data.Text hiding (dropWhile, reverse, map, splitOn)
import System.Directory
import Data.List.Split
import Data.Char (isSpace)


testDirPath :: FilePath
testDirPath = "E:\\Music\\Electronic"

fetchFilenames :: IO [FilePath]
fetchFilenames = listDirectory testDirPath

trim :: String -> String
trim = f . f
          where f = reverse . dropWhile isSpace

parseCandidatesFromTitle :: String -> [String]
parseCandidatesFromTitle s = map (\s -> trim s) (splitOn "-" s)