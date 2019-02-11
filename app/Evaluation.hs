module Evaluation where

import qualified Actions as A
import JavaParser (runParserOnPath, FileParseError(..))
import qualified CommandParser as P
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output)
import Output

import Data.List (intercalate)
import Control.Lens

processCommand :: String -> AppState -> IO AppState
processCommand input state =
  case P.runParser input of
    Left err -> return $ state & output .~ Error (putStrLn err)
    Right command -> eval command <$> evalMeta command state

eval :: P.Command -> AppState -> AppState
eval P.Exit = set output $ Other $ putStr "Done!"
eval P.Empty = id
eval (P.Meta _) = id
eval (P.Single P.Read) = A.read
eval (P.Single P.List) = A.listSelectedElements []
eval (P.Single P.Focus) = A.focusFirstElement
eval (P.Double P.List elementType) = A.listSelectedElements [JA.matchesType elementType]
eval (P.TermDouble P.List elementType term) = A.listSelectedElements [JA.allSatisfied [JA.matchesType elementType, JA.matchesTerm term]]
eval (P.TermSingle P.List term) = A.listSelectedElements [JA.matchesTerm term]
eval (P.TermDouble P.Focus elementType term) = A.focusFirstSelectedElementOfType elementType term
eval (P.TermSingle P.Focus term) = A.focusFirstSelectedElement term
eval (P.Double P.Focus elementType) = A.focusFirstElementOfType elementType
eval (P.PathSingle P.Focus P.Upper) = A.focusUp
eval (P.PathSingle P.Focus P.Root) = A.focusRoot
eval (P.Index number) = A.focusLastOutputByIndex (fromInteger number)
eval (P.IndexSingle P.Focus number) = A.focusLastOutputByIndex (fromInteger number)
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
