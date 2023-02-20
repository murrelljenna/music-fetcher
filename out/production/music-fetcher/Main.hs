{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

main :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
main = runReq defaultHttpConfig $ do
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      GET -- method
      (https "musicbrainz.org" /: "ws" /: "2" /: "artist" /: "?query-name:pink") -- safe by construction URL
      NoReqBody
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  liftIO $ print (responseBody r :: Value)
