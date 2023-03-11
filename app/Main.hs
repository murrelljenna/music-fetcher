{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models(FinalResult(..), refineDiscography, recordingArtist)
import FileInput
import Candidate

type MaybeFinalResult = Maybe FinalResult

processCandidate :: Candidate -> Req MaybeFinalResult
processCandidate (Candidate artistName t _) = do
  a <- fetchArtist artistName
  _ <- liftIO $ threadDelay 1000000
  discography <- fetchArtistDiscography (a, t)
  let mostLikelyRecording = refineDiscography t discography
  liftIO $ return $ (\r -> FinalResult a r) <$> mostLikelyRecording
processCandidate (TitleOnly title _) = do
  discography <- fetchDiscography title
  let mostLikelyRecording = refineDiscography title discography
  liftIO $ return $ (\r -> FinalResult (recordingArtist r) r) <$> mostLikelyRecording

processUntilSuitableResult :: MaybeFinalResult -> Candidate -> Req MaybeFinalResult
processUntilSuitableResult Nothing c = processCandidate c
processUntilSuitableResult result _ = pure result

processCandidates :: [Candidate] -> Req MaybeFinalResult
processCandidates cs = foldM processUntilSuitableResult Nothing cs

main :: IO ()
main = runReq defaultHttpConfig $ do
  mp3s <- liftIO $ fetchFilenames >>= \paths -> return $ (parseCandidates) <$> paths
  let candidates =  filter (not . null) mp3s
  results <- sequence $ processCandidates <$> candidates
  liftIO $ print results
