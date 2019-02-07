{-# LANGUAGE TemplateHaskell #-}

module AppState where

import qualified Java as J
import Focus
import PromptShow
import Control.Lens
import System.Console.ANSI

data Output =
  ResultList [J.Element] |
  Other (IO ()) |
  Error (IO ())
  
data AppState =
  AppState { _program :: J.Project
           , _focus :: Focus
           , _lastOutput :: Output
           , _output :: Output
           }

makeLenses ''AppState

-- Yields the element of the focus which is most down in the hierarchy
leafFocus :: AppState -> J.Element
leafFocus state =
  case state ^. focus of
    [] -> J.EProject $ state ^. program
    leaf:_ -> leaf

-- Sets the last output to the current output if it is not an error and clears the current output
clearOutput :: AppState -> AppState
clearOutput state = state
  & lastOutput .~ newLastOutput (state ^. output)
  & output .~ Other mempty
  where
    newLastOutput (Error io) = state ^. lastOutput
    newLastOutput newOutput = newOutput

printOutput :: AppState -> IO ()
printOutput state =
  case state ^. output of
    ResultList elements -> putStr $ unlines $ withIndex $ printSignature <$> elements
    Other io -> io
    Error io -> do
      setSGR [SetColor Foreground Vivid Red]
      io
      setSGR [Reset]
  where
    withIndex :: [String] -> [String]
    withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]
              
initialState :: AppState
initialState =
  AppState { _program = J.Project { J._javaFiles = [] }
           , _focus = []
           , _lastOutput = Other mempty
           , _output = Other mempty
           }
