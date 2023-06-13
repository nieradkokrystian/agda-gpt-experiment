{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# OPTIONS_GHC -fno-cse #-}

module Main  where

import Types
import qualified Gpt as G
import Extra
import AgdaApi


import System.Console.CmdArgs
import System.Environment (getArgs)
import System.Process
import System.Console.ANSI
import System.Exit
import System.FilePath (splitFileName)
import System.Directory

import Data.Aeson as A
import Control.Monad.Trans.RWS
import Control.Monad.Reader

import Control.Concurrent
import System.Environment

import System.IO

-- main :: IO ()
-- main = do
--   loadConfigAndRun  mainAG

main :: IO ()
main = do
  lcR mainAG


lcR :: (AGEnv  -> IO ()) -> IO ()
lcR  mainAG = do
  pwd  <- getEnv "PWD"
  args <- cmdArgs readArgs
  fPGpt <- check_promt "f"
  rPGpt <- check_promt "r"
  conf <- check_config (conF args)
  config  <- (A.decodeFileStrict conf) :: IO (Maybe FromConfig)
  let m = case  mode args of
                      "Pretty" -> PrettyMode
                      _        -> DebugMode
  case config of
    Nothing -> do
     cPrint  ("\nConfig file seems to be incorrect check it:  \n" ++ conf)  Red
     putStrLn "--"
     die "Something went wrong, try one more time"
    Just c -> do
      problemlist <- runReaderT buildProblemList (args, (problemsDir c))
      writeFile (pwd++"/aga-log.txt") ( "Aga has  " ++ (show (length problemlist)))
      let mainDir = pwd ++ "/aga-exec"
      createDirectory $ mainDir
      mapM_ (cAGE mainDir) problemlist
      where 
         cAGE dir problem = do
           let dirN = dir  ++ (nameP problem)
               newAF = "AGA-"++ "problem"
           putStrLn $show $  nameP problem     
           putStrLn dirN
           writeFile (dirN++"/Problem.agda") (agdaP problem)

           let  env = AGEnv
                 { apiKey = gptApiKey c
                 , orgAgdaF = dirN ++ "/Problem.agda"
                 , dirName = dirN
                 , agdaFile = newAF
                 , taskDescription = taskP problem
                 , operationMode = m
                 , maxTurns = maxT args
                 , fGptTemp = fPGpt
                 , rGptTemp = rPGpt
                 , gptModel =  gpt_model c
                 , tc_url = typeCheckerURL c
                 , tc_key = typeCheckerKEY c
                 }
           mainAG env




loadConfigAndRun :: (AGEnv  -> IO ()) -> IO ()
loadConfigAndRun mainAG = do
  args <- cmdArgs readArgs
  fPGpt <- check_promt "f"
  rPGpt <- check_promt "r"
  agda <- check_agda "fr"
  conf <- check_config (conF args)
  config  <- (A.decodeFileStrict conf) :: IO (Maybe FromConfig)
  ts <- timestamp
  let md = mode args
  case config of
    Nothing -> do
     cPrint  ("\nConfig file seems to be incorrect check it:  \n" ++ conf)  Red
     putStrLn "--"
     die "Something went wrong, try one more time"
    Just c ->
             let
             (path, file) =  splitFileName agda
             newAF = "AGA-"++ file
             pureF = take (length file - 5 )file
             dirN = pureF ++"_"++ts
             m = case md of
                      "Pretty" -> PrettyMode
                      _        -> DebugMode


             env = AGEnv
               { apiKey = gptApiKey c
               , orgAgdaF = agda
               , dirName = dirN
               , agdaFile = newAF
               , taskDescription = "to change"
               , operationMode = m
               , maxTurns = maxT args
               , fGptTemp = fPGpt
               , rGptTemp = rPGpt
               , gptModel =  gpt_model c
               , tc_url = typeCheckerURL c
               , tc_key = typeCheckerKEY c
               }
            in mainAG env


mainAG :: AGEnv -> IO ()
mainAG env = do
  checkAgdaF <- G.tryToCompile $  (orgAgdaF env) 
  case checkAgdaF of
    Just x -> do
       cPrint  ("Incorrect  agda File:  " ++ (orgAgdaF env) ++ "\n\n" ++ "COMPILER ERROR: " ++ x ) Red 
    Nothing -> do
               initInfo env
               threadDelay 1500000
               copyFile (orgAgdaF env) ((agdaFile env))
               createDirectory (dirName env)
               conversation env []


conversation :: AGEnv -> [ConvPart] -> IO ()
conversation env cP = do
  (mValue, state, _ ) <- runRWST G.debugMode env cP
  let l = length state
  case mValue of
    Just x ->
      if l  < (maxTurns env)
      then
        do
          conversation env state
        else do
        cPrint "Too many attempts, Agda-GPT-Assistan fail. Increase max turn or change agda task for GPT. \n Check logs files." Red
    Nothing ->do
      setSGR [(SetColor Foreground Dull Green)]
      clearScreen
      setCursorPosition 0 0
      putStrLn $ "Compilation succeeded in " ++ (show l) ++ " attempts. Check new "++ (agdaFile env) ++ " file\n\n" ++
        "Here is the code you need to add to your existing code:\n" ++
        (gpt_res (head state)) ++ "\n\nHere is the complite agda file code: \n\n" ++
        (current_agad_file (head state))
      setSGR [Reset]

initInfo :: AGEnv ->  IO ()
initInfo env = do
  clearScreen
  setCursorPosition 0 0 
  setSGR [(SetColor Foreground Dull Blue)]
  putStrLn "\n\n\n###############################################"
  putStrLn "Agda-GPT-Assistant started with the following flags:\n\n"
  setSGR [Reset]
  putStrLn $ "TASK:  " ++ (taskDescription env) ++ "\n\n"
  putStrLn $ "MODE:  " ++ (show (operationMode env)) ++ "\n\n"
  putStrLn $ "MAX TURN :  " ++ (show (maxTurns env)) ++ "\n\n"
  putStrLn $ "MODEL:  " ++ (gptModel env) ++ "\n\n"

