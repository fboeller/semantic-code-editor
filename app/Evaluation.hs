module Evaluation where

import qualified Actions as A
import JavaParser (runParserOnPath, FileParseError(..))
import qualified CommandParser as P
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output)
import Output

import Data.List (intercalate)
import Data.Maybe (maybe)
import Control.Lens

processCommand :: String -> AppState -> IO AppState
processCommand input state =
  case P.runParser input of
    Left err -> return $ state & output .~ Error (putStrLn err)
    Right command -> eval command <$> evalMeta command state

eval :: P.Command -> AppState -> AppState
eval P.Exit = A.exit
eval P.Empty = id
eval (P.Meta _) = id
eval (P.Double P.Read []) = A.read
eval (P.Double P.List []) = A.listSelectedElements [pure True]
eval (P.Double P.Focus []) = A.focusFirstElement
eval (P.Double P.Focus [(Just elementType,_)]) = A.focusFirstElementOfType elementType
eval (P.Double P.List criteria) = A.listSelectedElements $ JA.allSatisfied <$> (\(maybeType, maybeTerm) -> [maybe (pure True) JA.matchesType maybeType, maybe (pure True) JA.matchesTerm maybeTerm]) <$> criteria
eval (P.PathSingle P.Focus P.Upper) = A.focusUp
eval (P.PathSingle P.Focus P.Root) = A.focusRoot
eval (P.IndexSingle P.Focus numbers) = A.focusLastOutputByIndex (fromInteger <$> numbers)
eval input = set output $ Error $ putStrLn $ "The command '" ++ show input ++ "' is not yet implemented"

evalMeta :: P.Command -> AppState -> IO AppState
evalMeta (P.Meta (P.LoadFile path)) state = processJavaInput path state
evalMeta _ state = return state

processJavaInput :: String -> AppState -> IO AppState
processJavaInput path state = do
  (errors, javaProgram) <- runParserOnPath path
  return $ state
    & output .~ Error (putStr $ printErrors errors)
    & program .~ javaProgram & focus .~ []

printErrors :: [FileParseError] -> String
printErrors =
  intercalate "\n" . fmap printError
  where
    printError (FileParseError path error) = "File could not be loaded: " ++ path ++ " " ++ error
