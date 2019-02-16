module Commands.AutoComplete
  ( commandCompletion
  ) where

import Commands.LongParser
import Commands.Types

import Data.List (isPrefixOf)
import System.Console.Haskeline
import Control.Lens

commandCompletion :: CompletionFunc IO
commandCompletion = completeWordWithPrev Nothing [' '] possibilities

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

completesTo :: String -> Completion -> Bool
completesTo text completion = text `isPrefixOf` (replacement completion)

allDocumentedCompletions :: [Completion]
allDocumentedCompletions = concat $
  [ documentedCompletion <$> firstCommands
  , documentedCompletion <$> metaCommands
  ]

possibilities :: String -> String -> IO [Completion]
possibilities "" "" = return allDocumentedCompletions
possibilities "" enteredText = return $ filter (enteredText `completesTo`) allDocumentedCompletions
possibilities preCursorText enteredText
  | "list" `isPrefixOf` reverse preCursorText = return $ filter (enteredText `completesTo`) (documentedCompletion <$> elementTypes)
possibilities _ _ = return []

