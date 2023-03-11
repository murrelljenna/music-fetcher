module Candidate(parseCandidates, Candidate(..)) where

import FileInput
import Data.Char (isSpace)
import Data.List.Split

data Candidate = Candidate String String String | TitleOnly String String deriving (Eq, Show)

trimWhitespace :: String -> String
trimWhitespace = f . f
          where f = reverse . dropWhile isSpace

trimQuotes :: String -> String
trimQuotes t = case (head t, last t) of
  ('\'', '\'') -> (init . tail) t
  _ -> t

trim :: String -> String
trim = trimQuotes . trimWhitespace

parseOnDash :: Title -> [Candidate]
parseOnDash t = case (splitOn "-" t) of
  [x, y] -> [Candidate (trim x) (trim y) t]
  _ -> []

noArtist :: Title -> [Candidate]
noArtist t = [TitleOnly t t]

allCandidateParsers :: [Title -> [Candidate]]
allCandidateParsers = [parseOnDash, noArtist]

parseCandidates :: Title -> [Candidate]
parseCandidates t = foldl (\c f -> case c of
  [] -> f t
  cs -> cs) [] allCandidateParsers