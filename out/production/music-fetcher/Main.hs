{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models(FinalResult(..), refineDiscography)
import Input

type MaybeFinalResult = Maybe FinalResult

processCandidate :: Candidate -> Req MaybeFinalResult
processCandidate (Candidate artist title _) = do
  artist <- fetchArtist artist
  discography <- fetchArtistDiscography (artist, title)
  let mostLikelyRecording = refineDiscography title discography
  liftIO $ return $ (\r -> FinalResult (Just artist) r) <$> mostLikelyRecording
processCandidate (TitleOnly title _) = do
  discography <- fetchDiscography title
  let mostLikelyRecording = refineDiscography title discography
  liftIO $ return $ (\r -> FinalResult Nothing r) <$> mostLikelyRecording

--processCandidate (TitleOnly title _) = do

processCandidates :: [Candidate] -> Req MaybeFinalResult
processCandidates cs = processCandidate $ head cs

main :: IO ()
main = runReq defaultHttpConfig $ do
  mp3s <- liftIO $ fetchFilenames >>= \paths -> return $ (parseCandidatesFromTitle) <$> paths
  let artistNames = head <$> filter (not . null) mp3s
  _ <- liftIO $ print artistNames
  results <- sequence $ processCandidate <$> artistNames
  liftIO $ print results
