{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Req
import MusicBrainzClient
import Models(FinalResult(..), recordingArtist)
import FileInput
import Preprocess
import Postprocess

processCandidate :: Candidate -> Req (Maybe FinalResult)
processCandidate (Candidate artistName t _) = do
  a <- fetchArtist artistName
  _ <- liftIO $ threadDelay 1000000
  discography <- fetchArtistDiscography (a, t)
  let mostLikelyRecording = checkResult t discography
  liftIO $ return $ (\r -> FinalResult a r) <$> mostLikelyRecording
processCandidate (TitleOnly title _) = do
  discography <- fetchDiscography title
  let mostLikelyRecording = checkResult title discography
  liftIO $ return $ (\r -> FinalResult (recordingArtist r) r) <$> mostLikelyRecording

processUntilSuitableResult :: Maybe FinalResult -> Candidate -> Req (Maybe FinalResult)
processUntilSuitableResult Nothing c = processCandidate c
processUntilSuitableResult result _ = pure result

processCandidates :: [Candidate] -> Req (Maybe FinalResult)
processCandidates cs = foldM processUntilSuitableResult Nothing cs

main :: IO ()
main = runReq defaultHttpConfig $ do
  mp3s <- liftIO $ fetchFilenames >>= \paths -> return $ (parseCandidates) <$> paths
  let candidates =  filter (not . null) mp3s
  results <- sequence $ processCandidates <$> candidates
  liftIO $ print results
  
-- app :: String -> FinalResult
