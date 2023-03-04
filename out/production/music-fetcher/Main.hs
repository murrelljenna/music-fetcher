{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models
import Data.Text
import Input

main :: IO ()
main = runReq defaultHttpConfig $ do
  artistResponse <- fetchArtists "Pink Floyd"
  _ <- liftIO $ fetchFilenames >>= print
  discography <- fetchArtistDiscography (firstArtist artistResponse) "Wish you were here"
  let refinedDiscography = refineDiscography discography
  liftIO $ print refinedDiscography
