{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Api

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- fetchArtists "Pink Floyd"
  liftIO $ print r
