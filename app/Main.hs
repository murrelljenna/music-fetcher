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
  let titles = head <$> filter (not . null) mp3s
  _ <- liftIO $ print titles
  artistResponse <- fetchArtists $ fst <$> titles
  --discography <- fetchArtistDiscography (firstArtist artistResponse) "Wish you were here"
  --let refinedDiscography = refineDiscography discography
  liftIO $ print artistResponse
