module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output)

focusClass :: AppState -> AppState
focusClass state = over focus changeFocus state
  where changeFocus oldFocus = J.EClass $ head $ JA.classes oldFocus

