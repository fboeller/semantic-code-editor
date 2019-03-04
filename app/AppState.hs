{-# LANGUAGE TemplateHaskell #-}

module AppState where

import qualified Java.Types as J
import Focus
import Output
import Configuration

import Control.Lens
import Data.Tree (Tree)

data AppState =
  AppState { _project :: J.Project
           , _focus :: Focus
           , _lastResultTree :: Maybe (Tree J.Element)
           , _output :: Output
           , _running :: Bool
           , _config :: Configuration
           }

makeLenses ''AppState

-- Yields the element of the focus which is most down in the hierarchy
leafFocus :: AppState -> J.Element
leafFocus state =
  case state ^. focus of
    [] -> J.EProject $ state ^. project
    leaf:_ -> leaf

-- Sets the last output to the current output if it is not an error and clears the current output
clearOutput :: AppState -> AppState
clearOutput state = state
  & lastResultTree .~ newLastOutput (state ^. output)
  & output .~ Other mempty
  where
    newLastOutput (ResultTree tree) = Just tree
    newLastOutput _ = state ^. lastResultTree
              
initialState :: AppState
initialState =
  AppState { _project = J.Project { J._srcDir = "/", J._javaFiles = [] }
           , _focus = []
           , _lastResultTree = Nothing
           , _output = Other mempty
           , _running = True
           , _config = initialConfig
           }
