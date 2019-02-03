module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output)
import PromptShow

read :: AppState -> AppState
read state = set output (show $ state ^. program) state

listClasses :: AppState -> AppState
listClasses state = set output (unlines $ printClassSignature <$> JA.classes (state ^. focus)) state

listSelectedClasses :: String -> AppState -> AppState
listSelectedClasses term state = set output (unlines $ printClassSignature <$> JA.selectedClasses term (state ^. focus)) state

focusClass :: AppState -> AppState
focusClass state = over focus changeFocus state
  where changeFocus oldFocus = J.EClass $ head $ JA.classes oldFocus

