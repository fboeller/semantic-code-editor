module Commands.AutoComplete
  ( commandCompletion
  ) where

import Data.List (isPrefixOf)
import System.Console.Haskeline

commandCompletion :: CompletionFunc IO
commandCompletion = completeWordWithPrev Nothing [' '] possibilities

documentedCompletion :: String -> String -> Completion
documentedCompletion replacement display =
  Completion { replacement = replacement
             , display = display
             , isFinished = True
             }

firstCommands :: [Completion]
firstCommands = uncurry documentedCompletion <$>
  [ ("read", "read (Print the content of an element)")
  , ("focus", "focus (Focus a given element)")
  , ("list", "list (List a tree of elements matching the given selector)")
  ]

metaCommands :: [Completion]
metaCommands = uncurry documentedCompletion <$>
  [ (":load", ":load (Load a Java file or directory containing Java files)")
  , (":switch", ":switch (Switch the mode of the command parser to accept either long or short commands)")
  ]

completesTo :: String -> Completion -> Bool
completesTo text completion = text `isPrefixOf` (replacement completion)

possibilities :: String -> String -> IO [Completion]
possibilities "" "" = return $ firstCommands ++ metaCommands
possibilities "" enteredText =
  return $ filter (enteredText `completesTo`) (firstCommands ++ metaCommands)
possibilities _ _ = return []

