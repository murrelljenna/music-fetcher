{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api
import Models

main :: IO ()
main = runReq defaultHttpConfig $ do
  artistResponse <- fetchArtists "Pink Floyd"
  discography <- fetchArtistDiscography $ firstArtist artistResponse
  liftIO $ print discography
