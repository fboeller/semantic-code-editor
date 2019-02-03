module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, leafFocus)
import qualified Focus as F
import PromptShow

read :: AppState -> AppState
read state = set output (printCommon $ state ^. to leafFocus) state

listClasses :: AppState -> AppState
listClasses state = set output (unlines $ printClassSignature <$> JA.classes (state ^.to leafFocus)) state

listSelectedClasses :: String -> AppState -> AppState
listSelectedClasses term state = set output (unlines $ printClassSignature <$> JA.selectedClasses term (state ^.to leafFocus)) state

focusClass :: AppState -> AppState
focusClass state = over focus changeFocus state
  where changeFocus oldFocus = F.focusDown (J.EClass $ head $ JA.classes (state ^.to leafFocus)) oldFocus

focusUp :: AppState -> AppState
focusUp state = over focus F.focusUp state

listVariables :: AppState -> AppState
listVariables state = set output (unlines $ printFieldSignature <$> JA.variables (state ^.to leafFocus)) state
