module Lib
    ( someFunc,
    parseCandidatesFromTitle
    ) where

import Data.List.Split

someFunc :: IO ()
someFunc = putStrLn "a"

parseCandidatesFromTitle :: String -> [String]
parseCandidatesFromTitle s = splitOn " " s
