{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models(FinalResult(..), refineDiscography, recordingArtist)
import FileInput
import Candidate

type MaybeFinalResult = Maybe FinalResult

processCandidate :: Candidate -> Req MaybeFinalResult
processCandidate (Candidate artist title _) = do
  artist <- fetchArtist artist
  discography <- fetchArtistDiscography (artist, title)
  let mostLikelyRecording = refineDiscography title discography
  liftIO $ return $ (\r -> FinalResult artist r) <$> mostLikelyRecording
processCandidate (TitleOnly title _) = do
  discography <- fetchDiscography title
  let mostLikelyRecording = refineDiscography title discography
  liftIO $ return $ (\r -> FinalResult (recordingArtist r) r) <$> mostLikelyRecording

processCandidates :: [Candidate] -> Req MaybeFinalResult
processCandidates cs = processCandidate $ head cs

main :: IO ()
main = runReq defaultHttpConfig $ do
  mp3s <- liftIO $ fetchFilenames >>= \paths -> return $ (parseCandidates) <$> paths
  let artistNames = head <$> filter (not . null) mp3s
  _ <- liftIO $ print artistNames
  results <- sequence $ processCandidate <$> artistNames
  liftIO $ print results
