module Evaluation where

import qualified Actions as A
import Java.Parser (runParserOnPath, FileParseError(..))
import qualified Commands.Parser as P
import qualified Commands.Types as P
import qualified Java.Accessors as JA
import AppState (AppState, program, focus, output, config)
import Output
import Configuration (commandParserType)

import Data.List (intercalate)
import Data.Maybe (maybe)
import Control.Applicative (liftA2)
import Control.Lens

processCommand :: String -> AppState -> IO AppState
processCommand input state =
  case P.runParser (state ^. config . commandParserType) input of
    Left err -> return $ state & output .~ Error (putStrLn err)
    Right command -> eval command <$> evalMeta command state

eval :: P.Command -> AppState -> AppState
eval P.Exit = A.exit
eval P.Empty = id
eval (P.Meta _) = id
eval (P.Double P.Read []) = A.readFocus
eval (P.Double P.List []) = A.listSelectedElements [pure True]
eval (P.Double P.Focus []) = A.focusFirstElement
eval (P.Double P.Focus [(Just elementType, Nothing)]) = A.focusFirstElementOfType elementType
eval (P.Double P.Focus [(Nothing, Just term)]) = A.focusFirstSelectedElement term
eval (P.Double P.List criteria) = A.listSelectedElements $
  allSatisfied . (\(maybeType, maybeTerm) -> [maybe (pure True) JA.matchesType maybeType, maybe (pure True) JA.matchesTerm maybeTerm]) <$> criteria
eval (P.PathSingle P.Focus P.Upper) = A.focusUp
eval (P.PathSingle P.Focus P.Root) = A.focusRoot
eval (P.IndexSingle P.Focus numbers) = A.focusLastOutputByIndex (fromInteger <$> numbers)
eval (P.IndexSingle P.Read numbers) = A.readLastOutputByIndex (fromInteger <$> numbers)
eval input = set output $ Error $ putStrLn $ "The command '" ++ show input ++ "' is not yet implemented"

-- Creates a function that returns True iff all given functions return True.
allSatisfied :: [a -> Bool] -> a -> Bool
allSatisfied = foldr (liftA2 (&&)) (pure True)

evalMeta :: P.Command -> AppState -> IO AppState
evalMeta (P.Meta (P.LoadFile path)) state = processJavaInput path state
evalMeta (P.Meta (P.SwitchCommandParser parserType)) state = switchCommandParser parserType state
evalMeta _ state = return state

switchCommandParser :: P.ParserType -> AppState -> IO AppState
switchCommandParser parserType state =
  return $ state & config.commandParserType .~ parserType

processJavaInput :: String -> AppState -> IO AppState
processJavaInput path state = do
  (errors, javaProgram) <- runParserOnPath path
  return $ state
    & output .~ Error (putStr $ printErrors errors)
    & program .~ javaProgram
    & focus .~ []

printErrors :: [FileParseError] -> String
printErrors =
  intercalate "\n" . fmap printError
  where
    printError (FileParseError path error) = "File could not be loaded: " ++ path ++ " " ++ error
