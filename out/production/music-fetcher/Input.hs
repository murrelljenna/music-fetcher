{-# LANGUAGE OverloadedStrings #-}

module Input(parseCandidatesFromTitle, fetchFilenames, Title, Fragment, Candidate) where

import Data.Text hiding (dropWhile, reverse, map, splitOn, last, filter, init, concat)
import System.Directory
import Data.List.Split
import Data.Char (isSpace)

type Title = String
type Fragment = String
type Candidate = (String, String)

testDirPath :: FilePath
testDirPath = "C:\\Users\\Jenna\\Downloads\\testfiles"

fetchFilenames :: IO [FilePath]
fetchFilenames = trimExtension . filterForMp3 <$> listDirectory testDirPath

isMp3 :: FilePath -> Bool
isMp3 fp = last (splitOn "." fp) == "mp3"

filterForMp3 :: [FilePath] -> [FilePath]
filterForMp3 fps = filter isMp3 fps

trimExtension :: [FilePath] -> [FilePath]
trimExtension fps = (concat . init . (splitOn ".")) <$> fps

trim :: String -> String
trim = f . f
          where f = reverse . dropWhile isSpace

fragment :: Title -> String -> [Fragment]
fragment t sep = map (\s -> trim s) (splitOn sep t)

candidatesFromFragments :: [Fragment] -> [Candidate]
candidatesFromFragments [] = []
candidatesFromFragments [f1] = []
candidatesFromFragments [f1, f2] = [(f1, f2), (f2, f1)]
candidatesFromFragments [f1, f2, f3] = candidatesFromFragments [f1 <> f2, f3] ++ candidatesFromFragments [f1, f2 <> f3]
candidatesFromFragments [f1, f2, f3, f4] = candidatesFromFragments [f1 <> f2, f3 <> f4] ++ candidatesFromFragments [f1 <> f2 <> f3, f4]
candidatesFromFragments _ = []

parseCandidatesFromTitle :: Title -> [Candidate]
parseCandidatesFromTitle t = (candidatesFromFragments . fragment t) "-"