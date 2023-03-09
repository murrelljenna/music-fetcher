{-# LANGUAGE OverloadedStrings #-}

module FileInput(fetchFilenames, Title) where

import System.Directory
import Data.List.Split

type Title = String

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


