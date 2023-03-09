{-# LANGUAGE OverloadedStrings #-}

module Input(parseCandidatesFromTitle, fetchFilenames, Title, Fragment, Candidate(..), parseOnDash, candidateTitle, candidateArtist) where

import System.Directory
import Data.List.Split
import Data.Char (isSpace)

type Title = String
type Fragment = String
data Candidate = Candidate String String String | TitleOnly String String deriving (Eq, Show)

candidateArtist :: Candidate -> String
candidateArtist (Candidate a _ _) = a

candidateTitle :: Candidate -> String
candidateTitle (Candidate _ t _) = t
candidateTitle (TitleOnly t _) = t

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

trimWhitespace :: String -> String
trimWhitespace = f . f
          where f = reverse . dropWhile isSpace

trimQuotes :: String -> String
trimQuotes t = case (head t, last t) of
  ('\'', '\'') -> (init . tail) t
  _ -> t

trim :: String -> String
trim = trimQuotes . trimWhitespace

fragment :: Title -> String -> [Fragment]
fragment t sep = map (\s -> trimWhitespace s) (splitOn sep t)

parseOnDash :: Title -> [Candidate]
parseOnDash t = case (splitOn "-" t) of
  [x, y] -> [Candidate (trim x) (trim y) t]
  _ -> []

noArtist :: Title -> [Candidate]
noArtist t = [TitleOnly t t]

allCandidateParsers = [parseOnDash, noArtist]

parseCandidatesFromTitle :: Title -> [Candidate]
parseCandidatesFromTitle t = foldl (\c f -> case c of
  [] -> f t
  cs -> cs) [] allCandidateParsers