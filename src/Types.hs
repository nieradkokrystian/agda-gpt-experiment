{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# OPTIONS_GHC -fno-cse #-}

module Types  where

import Data.Aeson as A


import System.Console.CmdArgs
import Control.Monad.IO.Class

import Control.Monad.Trans.RWS
import Control.Monad.RWS.Class


data ChatCompletion = ChatCompletion
    { id :: String
    , object :: String
    , created :: Integer
    , model :: String
    , usage :: Usage
    , choices :: [Choice]
    } deriving (Show)

data Usage = Usage
    { prompt_tokens :: Int
    , completion_tokens :: Int
    , total_tokens :: Int
    } deriving (Show)

data Choice = Choice
    { message :: Message
    , finish_reason :: String
    , index :: Int
    } deriving (Show)

data Message = Message
    { role :: String
    , content :: String
    } deriving (Show)

instance FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" $ \v -> ChatCompletion
        <$> v .: "id"
        <*> v .: "object"
        <*> v .: "created"
        <*> v .: "model"
        <*> v .: "usage"
        <*> v .: "choices"

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \v -> Usage
        <$> v .: "prompt_tokens"
        <*> v .: "completion_tokens"
        <*> v .: "total_tokens"

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v -> Choice
        <$> v .: "message"
        <*> v .: "finish_reason"
        <*> v .: "index"

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "role"
        <*> v .: "content"

instance ToJSON Message where
  toJSON (Message role content) = A.object ["role" .= role, "content" .= content]




data OperationMode =  PrettyMode |  DebugMode
  deriving (Show)


data AGEnv = AGEnv
    { apiKey :: String
    , orgAgdaF :: String
    , dirName :: String
    , agdaFile :: FilePath
    , taskDescription :: String
    , operationMode :: OperationMode
    , maxTurns :: Int
    , fGptTemp :: FilePath
    , rGptTemp :: FilePath
    , gptModel :: String
    } deriving (Show)

data Aga = Aga { agda :: String
                 , task :: String
                 , conF :: String
                 , mode :: String
                 , maxT :: Int 
                 } deriving (Show, Data, Typeable)

readArgs :: Aga
readArgs =
  Aga{ agda = def &= help "This flag has no default value. Enter the file name of agda or the entire filepath, eg. Foo.agda" &= typFile
      , task = def &= help "This flag has no default value. Enter the function type, eg.  not : Bool → Bool " &= typ "SIGNATURE"
      , conF = "config.json" &=help  "this is a config file, it should be in the current directory or * ~/.agda-gpt-experiment * default value for this flag is config.json" &= typFile
      , mode = "Pretty" &= help "Choose one of the operating modes. * Pretty * or\n* Debug *  - which has more details. The default value for this flag is Pretty." &= typ "MODE" &= name "m"
      , maxT = 5 &= help "Set this flag to specify the number of round conversations with ChatGPT. This flag has a default value of 5." &= typ "NUMBER" &= name "l"
      } &= summary "##################### adga-gpt-experiment #####################\n\nBefore start, you have to prepare directory with templates. Directory is available in git repositry * /data/templates *  Copy this dir in your currnet executing dir, or * ~/.agda-gpt-experiment * witch one you need to create first. \n\nExample: aga -a=Test.agda -t=not : Bool → Bool -c=myConfig.json -m=Pretty -l=15" &= details ["More details on the website https://codecredence.ai"]




data FromConfig = FromConfig
  { gptApiKey :: String
  , gpt_model :: String
  } deriving (Show)

instance FromJSON FromConfig where
  parseJSON = withObject "Config" $ \v -> FromConfig
    <$> v .: "GPT_Api_key"
    <*> v .: "gpt_model"



data ConvPart = ConvPart
                  { gpt_input :: String
                  , gpt_res :: String
                  , pure_code_res :: String
                  , current_agad_file :: String
                  , agda_res :: Maybe String
                  , promptL :: [Message]
                  } deriving (Show)


type AGMonad  = RWST AGEnv () [ConvPart] IO  




