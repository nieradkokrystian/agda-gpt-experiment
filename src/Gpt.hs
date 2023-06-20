{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gpt where

import Types
import Extra
import AgdaApi

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

import Control.Concurrent


decodeRes :: BL.ByteString -> String
decodeRes r = case ((A.decode r ):: Maybe ChatCompletion ) of
                     Nothing -> "Somethig went wrong with GPT api request.."
                     Just x -> content $ message $ Prelude.head $ choices x

genJsonReq ::String -> [Message] -> BL.ByteString
genJsonReq model messages =
  encode $ A.object ["model" .=model, "messages" .= messages]


-- Create a request to the GPT API
createGptRequest :: String -> [Message] -> String -> Request
createGptRequest model prompt key = request
  where
    apiKey_ = B.pack key
    baseRequest = parseRequest_ "https://api.openai.com/v1/chat/completions"
    request = baseRequest
      { method = "POST"
      , requestHeaders = [ ("Content-Type", "application/json")
                         , ("Authorization", B.concat ["Bearer ", apiKey_])
                         ]
      , requestBody = RequestBodyLBS (genJsonReq model prompt)
      }



gptConv ::  String -> [Message] -> String -> OperationMode -> IO (String, String)
gptConv model prompt key oM= do
  let rprompt = L.reverse (trimPrompt prompt)
  manager <- newManager tlsManagerSettings
  let reqb = createGptRequest model rprompt key
  request <- return (createGptRequest model rprompt key)
  response <- httpLbs reqb manager
  let code = (statusCode $ responseStatus response)
  case code of
    200 -> do
      case oM of
        PrettyMode -> do
          -- putStrLn $ show (length prompt)
          -- mapM (\x -> putStrLn ((show x)++ "\n\n")) prompt
          return ()
        DebugMode -> do
          setSGR [(SetColor Foreground Dull Yellow)]
          putStrLn $ "\n\n\n" ++ show reqb ++ "\n\n\n\n\n"
          putStrLn $ show $ genJsonReq model rprompt
          putStrLn $ show $  response
          setSGR [Reset]
      return $  (plainCode  (decodeRes $ responseBody response),
                ( decodeRes $ responseBody response))
    _ -> do
      cPrint ("CODE: " ++ (show code) ++ "\n\n")  Red
      cPrint ( show response ) Red
      -- putStrLn "--"
      -- "Something went wrong, try one more time"
      return ("EMPTY!!!","EMPTY!!!")

      
plainCode :: String ->  String
plainCode res =
  let exR = extractCode  res
  in
  case exR of
    [] -> "EMPTY!!!" --  change into Maybe String
    _ -> rmSubS "Agda" (rmSubS "agda" (L.concat $ exR))


extractCode :: String -> [String]
extractCode str = extractCodeBlocks' str []
  where extractCodeBlocks' [] acc = acc
        extractCodeBlocks' xs acc = let block = takeCodeBlock xs
                                    in case block of
                                      Nothing -> extractCodeBlocks' (L.drop 1 xs) acc
                                      Just (code, rest) -> extractCodeBlocks' rest (acc ++ [code])

        takeCodeBlock :: String -> Maybe (String, String)
        takeCodeBlock xs = if L.take 3 xs == "```"
                           then let (block, rest) = L.span (/= '`') (L.drop 3 xs)
                                in Just (block, L.drop 3 rest)
                           else Nothing


debugMode :: AGMonad (Maybe String)
debugMode = do
  env <- ask
  state <- get
  x <- liftIO $ getCurrentDirectory
  let model = gptModel env
      key = apiKey env
      task = taskDescription env
      full = fullTask env
      mode = operationMode env
      dir = dirName env
      sl = L.length state
      agdafile =  (agdaFile env)
      a_log = (dir++"/agda.log")
      gA_log = (dir++"/all_gpt.log")
      gC_log = (dir++"/code_gpt.log")
      only_code = ("state_code_gpt.log")
      r = "\n\nREASPONSE: \n"
      p = "\n\nPROMPT: \n"
      at_info = "############## Attempt number:  " ++ show (sl+1) ++ "  ##############"
      firstPrompt = Message {role = "system", content = "You are a helpful assistant."}
  -- case mode of
  --   PrettyMode -> do
  --     return ()
  --   DebugMode -> do
  --     liftIO $ return ()


  if sl == 0
  then
    do
      liftIO $ clearScreen
      liftIO $ setCursorPosition 0 0
      liftIO $ cPrint ("###############  Initial problem  ###############\n") Cyan
      liftIO $ putStrLn $  full

      contentAfile <- liftIO $ readFile agdafile
      liftIO $ appendFile a_log  "#########################  Initial data  #########################\n\n"
      liftIO $ appendFile a_log  contentAfile
      fcon <- liftIO $ fConvInput env
      -- liftIO $ cPrint "The following prompt has been sent to GPT chat\n\n" Yellow
      -- liftIO $ putStrLn $ fcon ++ "\n\n"
      liftIO $ appendFile gC_log  at_info
      liftIO $ appendFile gA_log  at_info
      liftIO $ appendFile gA_log (p ++ fcon)
      let promptReq = Message {role = "user", content = fcon}
      answareFromGPT <- liftIO $ gptConv model [promptReq, firstPrompt] key mode
      liftIO $ appendFile gC_log  (r++(fst answareFromGPT))
      liftIO $ writeFile only_code (fst answareFromGPT)
      liftIO $ appendFile gA_log  (r++(snd answareFromGPT))
      let promptRes = Message {role = "assistant" , content = (snd answareFromGPT)}
      -- liftIO $ threadDelay 1000000
      -- liftIO $ clearScreen
      -- liftIO $ setCursorPosition 0 0
      liftIO $ clearScreen
      liftIO $ setCursorPosition 0 0
      liftIO $ cPrint at_info  Cyan
      -- liftIO $ cPrint "The following reasponse was received from GPT:" Yellow
      liftIO $ appendFile agdafile (fst answareFromGPT)
      newAfile <- liftIO $ readFile agdafile
      liftIO  $ writeFile only_code  at_info
      liftIO $ appendFile only_code (fst answareFromGPT)
      liftIO  $ appendFile a_log  at_info
      liftIO $ appendFile a_log ("\n\n" ++newAfile)
      -- case mode of
      --   DebugMode -> do
      --     liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
      --     liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
      --     liftIO $ putStrLn "Code Only"
      --     liftIO $ setSGR [Reset]
      
      --   PrettyMode -> do

      --     liftIO $ putStrLn $ fst answareFromGPT ++ "\n"
      --     liftIO $ threadDelay 1000000

      compiler <- liftIO $ tryToCompileAPI agdafile (meta_l env) (tc_url env)
      let newState = (createConvPart fcon answareFromGPT newAfile compiler [ promptRes, promptReq, firstPrompt]  : state)

      put newState
      -- liftIO $ threadDelay 3000000
      -- liftIO $ clearScreen
      -- liftIO $ setCursorPosition 0 0
      -- liftIO $ cPrint "New agda file, with GTP answare \n\n" Magenta
      -- liftIO $ putStrLn $ newAfile ++ "\n\n"

      case compiler of
        Nothing -> do
                   -- liftIO $ cPrint  (fst answareFromGPT) Red
                   -- liftIO $ threadDelay 3000000
                   liftIO $ cPrint  (trimAns (fst answareFromGPT)) Green
                   liftIO $ threadDelay 2000000 
                   return Nothing
        Just x -> do
                  -- liftIO $ threadDelay 3000000
                  -- liftIO $ clearScreen
                  -- liftIO $ setCursorPosition 0 0
                  -- liftIO $ cPrint (fst answareFromGPT) Red
                  -- liftIO $ threadDelay 3000000
                  liftIO $ cPrint( trimAns (fst answareFromGPT)) Red
                  -- liftIO $ threadDelay 2000000

                  return (Just x)

-- __________ >0

  else 
    do
      rcon <- liftIO $ rConvInput  (current_agad_file (L.head state))  env (fromJust(agda_res (L.head state)))
      -- liftIO $ threadDelay 3000000
      -- liftIO $ clearScreen
      -- liftIO $ setCursorPosition 0 0
      -- liftIO $ cPrint "The following prompt has been sent to GPT chat\n\n" Yellow
      -- liftIO $ putStrLn $ rcon ++ "\n\n"
      liftIO $ appendFile gA_log  ("\n\n"++ at_info)
      liftIO $ appendFile gA_log (p ++ rcon) 
      let rPromptReq = Message {role =  "user", content = rcon}
          sPrompt =  promptL (L.head state) 
      answareFromGPT <- liftIO $ gptConv model (rPromptReq : sPrompt) key  mode
      liftIO $ appendFile gA_log (r ++ (snd answareFromGPT))
      liftIO $ appendFile gC_log  ("\n\n"++ at_info)
      liftIO $ appendFile gC_log  (r++(fst answareFromGPT))
      liftIO  $ writeFile only_code  at_info
      liftIO $ appendFile only_code (fst answareFromGPT)
      let rPromptRes = Message {role = "assistant", content = (snd answareFromGPT)}
      -- liftIO $ threadDelay 1000000
      -- liftIO $ clearScreen
      -- liftIO $ setCursorPosition 0 0

 
      
      -- liftIO $ cPrint "The following reasponse was received from GPT:\n\n" Yellow 
      -- case mode of
      --   DebugMode -> do
      --     liftIO $ putStrLn $ snd answareFromGPT ++ "\n\n"
      --     liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
      --     liftIO $ putStrLn "Code Only"
      --     liftIO $ setSGR [Reset]
      --     liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"

      --   PrettyMode -> do
      --     liftIO $ putStrLn $ fst answareFromGPT ++ "\n\n"
      -- case (fst answareFromGPT) of
      --   "EMPTY!!!" -> do
      --      liftIO $ cPrint "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" Red
      --      liftIO $ putStrLn "ChatGPT hasn't responsed with any code"
      --   _ -> do
      --     -- liftIO $ rmAFile env
      --     liftIO $ removeFile agdafile
      --     liftIO $ copyFile (orgAgdaF env) (agdaFile env)
      --     liftIO $ appendFile agdafile (fst answareFromGPT)


      newAfile <- liftIO $ readFile agdafile
      compiler <- liftIO $ tryToCompileAPI agdafile (meta_l env) (tc_url env)
      liftIO  $ appendFile a_log  at_info
      liftIO $ appendFile a_log newAfile
      -- liftIO $ threadDelay 3000000
      -- liftIO $ clearScreen
      -- liftIO $ setCursorPosition 0 0
      -- liftIO $ cPrint "New agda file, with GTP answare \n\n" Magenta
      -- liftIO $ putStrLn newAfile
      let newState = (createConvPart rcon answareFromGPT newAfile compiler (rPromptRes:rPromptReq:sPrompt)  : state)
      put newState
      case compiler of
        Nothing -> do
                   liftIO $ clearScreen
                   liftIO $ setCursorPosition 0 0
                   liftIO $ cPrint at_info  Cyan   
                   liftIO $ cPrint  (trimAns(fst answareFromGPT)) Green
                   liftIO $ threadDelay 2000000
                   return Nothing
        Just x -> do
                  -- liftIO $ threadDelay 3000000
                  -- liftIO $ clearScreen
                  -- liftIO $ setCursorPosition 0 0
                  liftIO $ clearScreen
                  liftIO $ setCursorPosition 0 0
                  liftIO $ cPrint at_info  Cyan
                  liftIO $ cPrint (trimAns (fst answareFromGPT)) Red
                  -- liftIO $ threadDelay 3000000
                  return (Just x)



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createConvPart :: String -> (String, String) -> String -> Maybe String -> [Message] ->ConvPart
createConvPart gptIn gptOut afile acres fp =
  ConvPart{ gpt_input =gptIn
          , gpt_res =  fst gptOut
          , pure_code_res = snd gptOut
          , current_agad_file =  afile
          , agda_res = acres
          , promptL = fp
          }

fConvInput :: AGEnv -> IO String
fConvInput env = do
    templ <- readFile $ fGptTemp env
    agda <- readFile $  (agdaFile env)
    let x1 = replaceText templ "{function_type}" (taskDescription env)
    let x2 = replaceText x1 "{agda_code}" agda
    return x2

rConvInput :: String -> AGEnv -> String -> IO String
rConvInput cf env err = do
    templ <- readFile $ rGptTemp env
    agda <- readFile $  (agdaFile env)
    let x1 = replaceText templ "{agda_code_with_changes}" cf
    let x2 = replaceText x1 "{compiler_errors}"  err
    return x2


replaceText :: String -> String -> String -> String
replaceText [] _ _ = []
replaceText str search replace
  | L.take (L.length search) str == search = replace ++ replaceText (L.drop (L.length search) str) search replace
  | otherwise = L.head str : replaceText (L.tail str) search replace



replaceStringLoop :: String -> String -> String -> String
replaceStringLoop old new input =
    let (prefix, suffix) = breakSubstring old input
    in if suffix == "" then input
       else prefix ++ new ++ replaceStringLoop old new (drop (length old) suffix)

breakSubstring :: String -> String -> (String, String)
breakSubstring [] xs = ([], xs)
breakSubstring _ [] = ([], [])
breakSubstring str str'@(x:xs)
  | str `L.isPrefixOf` str' = ([], str')
  | otherwise = (x : prefix, suffix)
  where
    (prefix, suffix) = breakSubstring str xs


rmSubS :: String -> String -> String
rmSubS substr str = go str
  where go [] = []
        go s@(x:xs)
          | substr `L.isPrefixOf` s = L.drop (L.length substr) s
          | otherwise = x : go xs

trimAns :: String -> String
trimAns ans = unlines $ take 15 (lines ans) 
