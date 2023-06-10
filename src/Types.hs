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
import Control.Monad.Reader 


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
    , tc_url :: String
    , tc_key :: String
    , prob_dir :: FilePath
    } deriving (Show)

data Aga = Aga { input :: String
               , source :: String
               , conF :: String
               , mode :: String
               , maxT :: Int
               } deriving (Show, Data, Typeable)


data ResponseApi = ResponseApi{ output :: String
                              , status :: Int} deriving (Show)



instance FromJSON ResponseApi where
    parseJSON = withObject "ResponseApi" $ \v -> ResponseApi
        <$> v .: "output"
        <*> v .: "status"


data ProblemsInput = OneProblem | Dir | ListofProblem deriving (Show)






data FromConfig = FromConfig
  { gptApiKey :: String
  , gpt_model :: String
  , typeCheckerURL :: String
  , typeCheckerKEY :: String
  , problemsDir :: String
  } deriving (Show)

instance FromJSON FromConfig where
  parseJSON = withObject "Config" $ \v -> FromConfig
    <$> v .: "GPT_Api_key"
    <*> v .: "gpt_model"
    <*> v .: "tChecker_url"
    <*> v .: "tChecker_key"
    <*> v .: "problems_dir"



data ConvPart = ConvPart
                  { gpt_input :: String
                  , gpt_res :: String
                  , pure_code_res :: String
                  , current_agad_file :: String
                  , agda_res :: Maybe String
                  , promptL :: [Message]
                  } deriving (Show)

data LoProblems = LoProblems { agdaP :: String
                             , taskP :: String
                             , metaP :: String
                             } deriving (Show)


type AGMonad  = RWST AGEnv () [ConvPart] IO

type PLMonad = ReaderT (Aga, String) IO



readArgs :: Aga
readArgs =
  Aga{ input = def &= help "This flag defines the problems located in the agda-problem-repository for AGA to solve. It can have one of three values. It can point to a single problem - * problem *, the entire directory of problems including subdirectories and their contents - * dir *, or a list of selected problems from the repository placed in a .json file - * list *. This flag does not have a default value."
      , source = def &= help "The problem name always consists of the full path to the Agda file, starting from the Problems directory in the Agda problem base repository, for example, Problems/ExampleProblems/P1.agda. \n\nIf you specified a specific problem to solve, you must provide its path starting from the main Problems directory, following the general principle of pointing to a problem. \n\nIf you specified a list of problems, it should be located in your current directory and saved in a file named problemslist.json. The location of each problem on the list to be solved must adhere to the general guidelines for pointing to a problem, such as Problems/ExampleProblems/P1.agda.\n\nIf you chose dir as the input, you must specify an existing directory from the problem base. It can also be just the Problems directory, in which case all the problems in the problem repository will be indicated for solving."
      , conF = "config.json" &=help  "this is a config file, it should be in the current directory or * ~/.agda-gpt-experiment * default value for this flag is config.json" &= typFile
      , mode = "Pretty" &= help "Choose one of the operating modes. * Pretty * or\n* Debug *  - which has more details. The default value for this flag is Pretty." &= typ "MODE" &= name "m"
      , maxT = 5 &= help "Set this flag to specify the number of round conversations with ChatGPT for each problem. This flag has a default value of 5." &= typ "NUMBER" &= name "l"
      } &= summary "##################### adga-gpt-experiment #####################\n\nBefore start, you have to prepare directory with templates. Directory is available in git repositry * /data/templates *  Copy this dir in your currnet executing dix1r, or * ~/.agda-gpt-experiment * witch one you need to create first. \n\nExample: aga -i=dir -s= Problems/ExampleProblems/ -c=myConfig.json -m=Pretty -l=15" &= details ["More details on the website https://codecredence.ai"]

