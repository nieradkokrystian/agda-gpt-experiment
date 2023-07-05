{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module AgdaApi where

import Types


import Control.Monad.Trans.RWS
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Data.Aeson as A
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.Process as SP
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory
import System.Environment (getEnv)

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.HTTP.Client.MultipartFormData

import Control.Concurrent





tryToCompileAPI :: String -> String -> String -> IO (Maybe String)
tryToCompileAPI agda meta  url = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST"}
                               -- , requestHeaders = [ ("Content-Type", "multipart/form-data")]}
      partList = [ partFile "Problem.agda" agda
                 , partFile "Problem.json" meta
                 ]
  req <- formDataBody partList request
  response <- httpLbs req manager
  res  <- dec $ responseBody response
  -- let x = status res
  -- putStrLn  $ ( (output res))
  return res
  -- case (status res) of
  --   0 -> Nothing
  --   _ -> (Just (output res))


dec :: BL.ByteString -> IO (Maybe String)
dec re = do
  let r = A.decode re :: Maybe ResponseApi
  case r of
    Nothing -> return Nothing
    Just x -> do
      case status x of
        0 -> return Nothing
        _ -> return $ Just (output x)

