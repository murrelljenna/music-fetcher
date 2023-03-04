{-# LANGUAGE OverloadedStrings #-}

module Input(parseCandidatesFromTitle, fetchFilenames, Title, Fragment, Candidate) where

import Data.Text hiding (dropWhile, reverse, map, splitOn)
import System.Directory
import Data.List.Split
import Data.Char (isSpace)

type Title = String
type Fragment = String
type Candidate = (String, String)

testDirPath :: FilePath
testDirPath = "E:\\Music\\Electronic"

fetchFilenames :: IO [FilePath]
fetchFilenames = listDirectory testDirPath

trim :: String -> String
trim = f . f
          where f = reverse . dropWhile isSpace

fragment :: Title -> [Fragment]
fragment t = map (\s -> trim s) (splitOn "-" t)

candidatesFromFragments :: [Fragment] -> [Candidate]
candidatesFromFragments [] = []
candidatesFromFragments [f1] = []
candidatesFromFragments [f1, f2] = [(f1, f2), (f2, f1)]
candidatesFromFragments [f1, f2, f3] = candidatesFromFragments [f1 <> f2, f3] ++ candidatesFromFragments [f1, f2 <> f3]
candidatesFromFragments [f1, f2, f3, f4] = candidatesFromFragments [f1 <> f2, f3 <> f4] ++ candidatesFromFragments [f1 <> f2 <> f3, f4]
candidatesFromFragments _ = []

parseCandidatesFromTitle :: Title -> [Candidate]
parseCandidatesFromTitle t = (candidatesFromFragments . fragment) t