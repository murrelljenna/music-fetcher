module Lib
    (
    parseCandidatesFromTitle
    ) where

import Data.List.Split
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
          where f = reverse . dropWhile isSpace

parseCandidatesFromTitle :: String -> [String]
parseCandidatesFromTitle s = map (\s -> trim s) (splitOn "-" s)
