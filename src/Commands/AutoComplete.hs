module Commands.AutoComplete
  ( commandCompletion
  ) where

import Commands.LongParser
import Commands.Types

import Data.List (isPrefixOf)
import System.Console.Haskeline
import Control.Lens

commandCompletion :: CompletionFunc IO
commandCompletion = fallbackCompletion
  (completeWordWithPrev Nothing [' '] (\a b -> return (possibilities a b)))
  loadFileCompletion

loadFileCompletion :: CompletionFunc IO
loadFileCompletion (preCursorText, enteredText) =
     complete (preCursorText, enteredText)
     where complete = if ":load" `isPrefixOf` reverse preCursorText
             then completeFilename
             else noCompletion

documentedCompletion :: Keyword a -> Completion
documentedCompletion keyword =
  Completion { replacement = keyword ^. word
             , display = keyword ^. description
             , isFinished = True
             }

metaCommands :: [Keyword String]
metaCommands = keywordFromTuple <$>
  [ (":load", "", ":load (Load a Java file or directory containing Java files)")
  , (":switch", "", ":switch (Switch the mode of the command parser to accept either long or short commands)")
  ]

stdCommands :: [Keyword String]
stdCommands = keywordFromTuple <$>
  [ ("help", "", "help (Show a help text listing different options and examples)")
  , ("quit", "", "quit (Quit the program)")
  ]

completesTo :: String -> Completion -> Bool
completesTo text = (text `isPrefixOf`) . replacement

allDocumentedCompletions :: [Completion]
allDocumentedCompletions = concat
  [ documentedCompletion <$> stdCommands
  , documentedCompletion <$> firstCommands
  , documentedCompletion <$> metaCommands
  ]

ifEmpty :: [a] -> [a] -> [a]
ifEmpty list alternative = if null list then alternative else list

firstNonEmpty :: [[a]] -> [a]
firstNonEmpty = foldr ifEmpty []

possibilities :: String -> String -> [Completion]
possibilities "" "" = allDocumentedCompletions
possibilities "" enteredText = filter (enteredText `completesTo`) allDocumentedCompletions
possibilities preCursorText enteredText
  | "list" `isPrefixOf` reverse preCursorText = firstNonEmpty
    [ filter (enteredText `completesTo`) (documentedCompletion <$> elementTypes)
    , [simpleCompletion $ "\"" ++ enteredText ++ "\""]
    ]
possibilities _ _ = []

