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





tryToCompileAPI :: String -> String -> IO (Maybe String)
tryToCompileAPI fp url = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST"}
                               -- , requestHeaders = [ ("Content-Type", "multipart/form-data")]}
      partList = [ partFile "Problem.agda" "/home/kryn/.agda-gpt-experiment/Problem.agda"
                 , partFile "Problem.json" "/home/kryn/.agda-gpt-experiment/Problem.json"
                 ] 
  req <- formDataBody partList request 
  response <- httpLbs req manager
  res <- dec $ responseBody response

  return Nothing
  


dec :: BL.ByteString -> IO (Maybe String)
dec re = do
  let r = A.decode re :: Maybe ResponseApi
  case r of
    Nothing -> return Nothing
    Just x -> return $ Just (output x)

-- ResponseApi


-- tryToCompile :: String -> IO (Maybe String)
-- tryToCompile fp = do
--   let (path, file) =  splitFileName fp
--   aReq <- runProcess_ path file
--   p  <- getEnv "PWD"
--   case aReq of
--     Nothing -> return Nothing
--     Just re -> do
--                 return $ Just $ replaceStringLoop (p++"/") "" re
      
-- runProcess_ ::  FilePath -> String -> IO (Maybe String)
-- runProcess_ pwd afile = do
--   let cp = shell $ "agda" ++ " " ++ afile
--       ncp = cp { cwd = Just pwd
--                , std_out = CreatePipe
--                , std_err = CreatePipe
--                }            
--   (code, output, errorOutput) <- readCreateProcessWithExitCode ncp ""
--   let result = case code of
--                   ExitSuccess   -> Nothing
--                   ExitFailure _ -> Just output
--   return result





-- createGptRequest :: String -> [Message] -> String -> Request
-- createGptRequest model prompt key = request
--   where
--     apiKey_ = B.pack key 
--     baseRequest = parseRequest_ "https://api.openai.com/v1/chat/completions"
--     request = baseRequest
--       { method = "POST"
--       , requestHeaders = [ ("Content-Type", "application/json")
--                          , ("Authorization", B.concat ["Bearer ", apiKey_])
--                          ]
--       , requestBody = RequestBodyLBS (genJsonReq model prompt)
--       }



-- gptConv ::  String -> [Message] -> String -> OperationMode -> IO (String, String)
-- gptConv model prompt key oM= do
--   let rprompt = L.reverse (trimPrompt prompt) 
--   manager <- newManager tlsManagerSettings
--   let reqb = createGptRequest model rprompt key
--   request <- return (createGptRequest model rprompt key)
--   response <- httpLbs reqb manager
--   let code = (statusCode $ responseStatus response)
--   case code of
--     200 -> do
--       case oM of
--         PrettyMode -> do
--           -- putStrLn $ show (length prompt)
--           -- mapM (\x -> putStrLn ((show x)++ "\n\n")) prompt
--           return ()
--         DebugMode -> do
--           setSGR [(SetColor Foreground Dull Yellow)]
--           putStrLn $ "\n\n\n" ++ show reqb ++ "\n\n\n\n\n"
--           putStrLn $ show $ genJsonReq model rprompt      
--           putStrLn $ show $  response
--           setSGR [Reset]
--       return $  (plainCode  (decodeRes $ responseBody response),
--                 ( decodeRes $ responseBody response))
--     _ -> do
--       cPrint ("CODE: " ++ (show code) ++ "\n\n")  Red
--       cPrint ( show response ) Red
--       putStrLn "--"
--       die "Something went wrong, try one more time" 
