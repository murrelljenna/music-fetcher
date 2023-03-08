{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models
import Input

main :: IO ()
main = runReq defaultHttpConfig $ do
  mp3s <- liftIO $ fetchFilenames >>= \paths -> return $ (parseCandidatesFromTitle) <$> paths
  let artistNames = head <$> filter (not . null) mp3s
  artists <- fetchArtists $ candidateArtist <$> artistNames
  let artistsAndTitles = zip artists $ candidateTitle <$> artistNames
  _ <- liftIO $ print artistsAndTitles
  discography <- sequence $ fetchArtistDiscography <$> artistsAndTitles
  let refinedDiscography = refineDiscography <$> discography
  liftIO $ print refinedDiscography
