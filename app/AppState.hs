{-# LANGUAGE TemplateHaskell #-}

module AppState where

import qualified Java as J
import Focus
import PromptShow
import Output

import Control.Lens
import Data.Tree (Tree)

data AppState =
  AppState { _program :: J.Project
           , _focus :: Focus
           , _lastResultTree :: Maybe (Tree J.Element)
           , _output :: Output
           , _running :: Bool
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
  & lastResultTree .~ newLastOutput (state ^. output)
  & output .~ Other mempty
  where
    newLastOutput (ResultTree tree) = Just tree
    newLastOutput _ = state ^. lastResultTree
              
initialState :: AppState
initialState =
  AppState { _program = J.Project { J._javaFiles = [] }
           , _focus = []
           , _lastResultTree = Nothing
           , _output = Other mempty
           , _running = True
           }
