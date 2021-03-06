{-# LANGUAGE TupleSections #-}

module Evaluation where

import qualified Actions as A
import Java.Parser (runParserOnPath, FileParseError(..))
import qualified Commands.Parser as P
import qualified Commands.Types as P
import qualified Java.Accessors as JA
import qualified Java.Types as J
import AppState (AppState, project, focus, output, config)
import Output
import Configuration (commandParserType)
import GitHubLoader (load)

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (maybe, fromMaybe)
import Data.Bifunctor (first)
import Control.Lens

processCommand :: String -> AppState -> IO AppState
processCommand input state =
  case P.runParser (state ^. config . commandParserType) input of
    Left err -> return $ state & output .~ Error (putStrLn err)
    Right command -> eval command <$> evalMeta command state

eval :: P.Command -> AppState -> AppState
eval P.Exit = A.exit
eval P.Help = A.help
eval P.Empty = id
eval (P.Meta _) = id
eval (P.Double P.Read []) = A.readFocus
eval (P.Double P.Focus []) = A.focusFirstOfSelectedElements (pure True)
eval (P.Double P.Focus [criterium]) = A.focusFirstOfSelectedElements $ JA.matchesAllGiven criterium
eval (P.Double P.List []) = A.listSelectedElements [pure True]
eval (P.Double P.List criteria) = A.listSelectedElements $ JA.matchesAllGiven <$> criteria
eval (P.PathSingle P.Focus P.Upper) = A.focusUp
eval (P.PathSingle P.Focus P.Root) = A.focusRoot
eval (P.IndexSingle cmd numbers) = A.onLastOutputByIndex cmd (fromInteger <$> numbers)
eval input = set output $ Error $ putStrLn $ "The command '" ++ show input ++ "' is not yet implemented"

evalMeta :: P.Command -> AppState -> IO AppState
evalMeta (P.Meta (P.LoadFile path)) state = processJavaInput path state
evalMeta (P.Meta (P.SwitchCommandParser parserType)) state = switchCommandParser parserType state
evalMeta _ state = return state

switchCommandParser :: P.ParserType -> AppState -> IO AppState
switchCommandParser parserType state =
  return $ state & config.commandParserType .~ parserType

processJavaInput :: String -> AppState -> IO AppState
processJavaInput path state = do
  (errors, javaProject) <- loadJavaInput path
  return $ state
    & output .~ Error (putStr errors)
    & project %~ (<>javaProject)
    & focus .~ []

loadJavaInput :: String -> IO (String, J.Project)
loadJavaInput path =
  if "github:" `isPrefixOf` path then
    either (,J.Project { J._srcDir = path, J._javaFiles = [] }) ("",) <$> load path
  else
    first printErrors <$> runParserOnPath path


printErrors :: [FileParseError] -> String
printErrors =
  intercalate "\n" . fmap printError
  where
    printError (FileParseError path error) = "File could not be loaded: " ++ path ++ " " ++ error
