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
import Control.Monad as MM

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
import System.FilePath 

import Control.Concurrent

cPrint :: String -> Color -> IO ()
cPrint s c = do
  setSGR [(SetColor Foreground Dull c )]
  putStrLn s
  setSGR [Reset]



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



buildProblemList :: PLMonad [Problem]
buildProblemList = do
  fromReader <- MR.ask
  let dirP = snd fromReader
      inputP = input (fst fromReader)
      sourceP = source (fst fromReader)
  case inputP of
    "problem" -> do
      problem <-  liftIO $ extractProblem (dirP ++ sourceP ++ ".agda")
      return [problem]
    "dir" -> do
      problemsAL <- liftIO $ dirInspection  (dirP  ++ sourceP ++ "/") []
      problems <- liftIO $ mapM  extractProblem problemsAL
      return problems
    "list" -> do
      l <- liftIO $ decodeList sourceP
      let newl  = fmap (\x -> dirP ++ x) l
      x <- liftIO $ mapM extractProblem newl
      return x
    _ ->  do liftIO $ do
                        cPrint ("Type proper input value\n") Red
                        putStrLn "--"
                        die "Something went wrong, try one more time"


type  LString = [String]

dirInspection :: String ->  LString ->IO [String]
dirInspection dir list= do
  c <-listDirectory $ dir
  let newDir = fmap (\x -> dir ++ x) $ L.sort c
  let flist = L.foldl funcA list newDir
  dlist <-funcB  newDir (list)
  return $ dlist ++ flist
  where

    funcA acc l = case  takeExtension l of
      ".agda" -> l : (acc :: [String])
      _ -> acc

funcB :: LString -> LString -> IO LString
funcB dirs lstring = do

  neL <-  MM.foldM funcC lstring dirs
  return neL
  where
    funcC acc ll = do
      ex <- doesDirectoryExist ll
      case ex of
        True -> dirInspection (ll++ "/") acc
        False -> return acc

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



extractProblem :: String -> IO Problem
extractProblem fp = do
  pFile <- check_agda fp
  readedAFile <- liftIO $ readFile pFile
  let (task, agda, full) = splitProblem readedAFile
  meta <- findMetaD fp
  return $ Problem agda task meta fp full



findMetaD :: String -> IO FilePath
findMetaD fp = do
  let (path, file) =  splitFileName fp
  e <- doesFileExist $ replaceExtension fp "json"
  case e of
    True -> return fp
    False -> findMetaF path

findMetaF :: FilePath -> IO FilePath
findMetaF path = do
  let filePath = path ++ "Meta.json"
  m <- doesFileExist $ filePath
  case m of
    True -> return filePath
    False -> do
      findMetaF  $ (( takeDirectory .  takeDirectory) path) ++ "/"


splitProblem :: String -> (String, String, String)
splitProblem file =
  let list@(x:z:r) =( L.reverse . S.lines) file in
  (z,((S.unlines . L.reverse) r), (z ++ "\n" ++ x))


