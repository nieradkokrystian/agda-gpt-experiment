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

import qualified Data.String as S 

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text as T
import System.Exit
import System.FilePath (splitFileName, replaceExtension,  takeDirectory)

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


buildProblemList :: PLMonad [Problem]
buildProblemList = do
  fromReader <- MR.ask
  let dirP = snd fromReader
      inputP = input (fst fromReader)
      sourceP = source (fst fromReader)
  case inputP of
    "problem" -> do
      problem <-  liftIO $ extractProblem dirP sourceP
      return [problem]
    "dir" -> do
      liftIO rr
      return []
    "list" -> do
      l <- liftIO $ decodeList sourceP
      x <- liftIO $ mapM (extractProblem dirP) l
      liftIO $ putStrLn $ show x
      return x
    _ ->  do liftIO $ do
                        cPrint ("Type proper input value\n") Red
                        putStrLn "--"
                        die "Something went wrong, try one more time"
  return []




rr :: IO ()
rr = do
  c <-listDirectory "/home/kryn/ww"
  putStrLn $ show c


decodeList :: String -> IO [String]
decodeList name  =  do
  pwd <- getEnv "PWD"
  ls <- (A.decodeFileStrict (pwd ++ "/" ++ name)) :: IO ( Maybe [String])
  case ls of
    Nothing -> do
               cPrint ("Incorect problem list ") Red
               putStrLn "--"
               die "Something went wrong, try one more time"
    Just list -> return list



extractProblem :: String -> String -> IO Problem
extractProblem dir file = do
  let fp = dir ++ file
  pFile <- check_agda fp
  readedAFile <- liftIO $ readFile pFile
  let (task, agda) = splitProblem readedAFile
  meta <- findMetaD fp
  return $ Problem agda task meta



findMetaD :: String -> IO FilePath
findMetaD fp = do
  let (path, file) =  splitFileName fp
  e <- doesFileExist $ replaceExtension fp "json"
  -- putStrLn $ show e
  case e of
    True -> return fp
    False -> findMetaF path

findMetaF :: FilePath -> IO FilePath
findMetaF path = do
  let filePath = path ++ "Meta.json"
  -- putStrLn $ show filePath
  m <- doesFileExist $ filePath
  case m of
    True -> return filePath
    False -> do
      findMetaF  $ (( takeDirectory .  takeDirectory) path) ++ "/"


splitProblem :: String -> (String, String)
splitProblem file =
  let list@(x:z:r) =( L.reverse . S.lines) file in
  (z,((S.unlines . L.reverse) r))
