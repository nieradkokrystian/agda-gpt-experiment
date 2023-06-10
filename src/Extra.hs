{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Extra where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.RWS 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader as MR

import Data.Maybe

import  Types

import System.Environment (getEnv)

import System.Directory
import Data.Aeson as A 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process as SP
import qualified Data.List as L
import Data.List.Split as DLS(splitOn)


import Data.Time.Clock.POSIX
import Data.Time.Format 


import System.Console.ANSI


import Data.List.Split 


import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName)

import Control.Concurrent

cPrint :: String -> Color -> IO ()
cPrint s c = do
  setSGR [(SetColor Foreground Dull c )]
  putStrLn s
  setSGR [Reset]


-- cPrint :: String -> Color -> IO ()
-- cPrint s c = do
--   setSGR [(SetColor Foreground Dull c )]
--   clearScreen
--   threadDelay 7000000 -- microSec
--   setCursorPosition 0 0 
--   putStrLn s
--   setSGR [Reset]


check_promt :: String ->  IO String
check_promt s = do
  home <- getEnv "HOME"
  l <- doesFileExist $ "./templates/gpt_"++s++".txt"
  case l of
    True -> return $ "./templates/gpt_"++s++".txt"
    False -> do
      g <- doesFileExist $ home ++ "/.agda-gpt-experiment/templates/gpt_"++s++".txt"
      case g of
        True -> return  $ home++"/.agda-gpt-experiment/templates/gpt_"++s++".txt"
        False -> do
          cPrint ("I can't find gpt_"++s++".txt, check it in /templates/ or ~/.agda-gpt-experiment/templates\n") Red
          putStrLn "--"
          die "Something went wrong, try one more time"

check_agda :: String ->  IO String
check_agda file = do
  l <- doesFileExist $ file
  case l of
    True -> return file
    False -> do
       cPrint ("I can't find agda file: "++ file ) Red
       putStrLn "--"
       die "Something went wrong, try one more time"


check_config :: String -> IO String
check_config conf = do
  home <- getEnv "HOME"
  l <- doesFileExist $ conf
  case l of
    True -> return $ conf
    False -> do
      g <- doesFileExist $ home++"/.agda-gpt-experiment/" ++ conf 
      case g of
        True -> return  $ home++"/.agda-gpt-experiment/"++ conf
        False -> do
          cPrint ("I can't find " ++ conf ++ ", check it in currnet dir or ~/.agda-gpt-experiment/\n") Red
          putStrLn "--"
          die "Something went wrong, try one more time"



timestamp :: IO String
timestamp = do
  current <- getPOSIXTime
  let formatted = formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime current)
  return formatted


trimPrompt :: [Message] -> [Message]
trimPrompt ml =
  let l = L.length ml in 
  if (L.length ml) <= 16 then ml
  else
    let (p1:p2:x)= ml in
      p1 : p2 : (L.drop (l - 10) ml)


-- data ProblemsInput = OneProblem | Dir | ListofProblem deriving (Show)


buildProblemList :: PLMonad [LoProblems]
buildProblemList = do
  fromReader <- MR.ask
  let dirP = snd fromReader
      inputP = input (fst fromReader)
      sourceP = source (fst fromReader)
  case inputP of
    "problem" -> do
      problem <-  liftIO $ createProblem (dirP++sourceP)
      return problem
    "dir" -> return []
    "list" -> return []
    _ -> liftIO $ die "\n\nType  proper input value \n"
  return []


createProblem :: String -> IO [LoProblems]
createProblem filePath = do
  agda <- check_agda filePath
  return []

splitProblem :: String -> (String, String)
splitProblem file = undefined

findMeta :: String -> FilePath
findMeta file = undefined

  


-- data Aga = Aga { input :: String
--                , source :: String
--                , conF :: String
--                , mode :: String
--                , maxT :: Int
--                } deriving (Show, Data, Typeable)


-- type PLMonad = ReaderT (Aga, String) IO
