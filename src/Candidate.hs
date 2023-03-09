module Candidate(parseCandidates, Candidate(..)) where

import FileInput
import Data.Char (isSpace)
import Data.List.Split

type Fragment = String

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

fragment :: Title -> String -> [Fragment]
fragment t sep = map (\s -> trimWhitespace s) (splitOn sep t)

parseOnDash :: Title -> [Candidate]
parseOnDash t = case (splitOn "-" t) of
  [x, y] -> [Candidate (trim x) (trim y) t]
  _ -> []

noArtist :: Title -> [Candidate]
noArtist t = [TitleOnly t t]

allCandidateParsers = [parseOnDash, noArtist]

candidateArtist :: Candidate -> String
candidateArtist (Candidate a _ _) = a

candidateTitle :: Candidate -> String
candidateTitle (Candidate _ t _) = t
candidateTitle (TitleOnly t _) = t

parseCandidates :: Title -> [Candidate]
parseCandidates t = foldl (\c f -> case c of
  [] -> f t
  cs -> cs) [] allCandidateParsers