module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, leafFocus)
import qualified Focus as F
import PromptShow

read :: AppState -> AppState
read state = set output (putStrLn $ printCommon $ state ^. to leafFocus) state

listClasses :: AppState -> AppState
listClasses state = set output (putStr $ unlines $ printClassSignature <$> JA.classes (state ^.to leafFocus)) state

listMethods :: AppState -> AppState
listMethods state = set output (putStr $ unlines $ printMethodSignature <$> JA.methods (state ^.to leafFocus)) state

listVariables :: AppState -> AppState
listVariables state = set output (putStr $ unlines $ printFieldSignature <$> JA.variables (state ^.to leafFocus)) state

listSelectedClasses :: String -> AppState -> AppState
listSelectedClasses term state = set output (putStrLn $ unlines $ printClassSignature <$> JA.selectedClasses term (state ^.to leafFocus)) state

focusFirst :: (J.Element -> [a]) -> (a -> J.Element) -> AppState -> AppState
focusFirst subElements toElement state =
  over focus changeFocus state
  where
    changeFocus oldFocus = F.focusDown element oldFocus
    element = toElement $ head $ subElements (state ^.to leafFocus)

focusClass :: AppState -> AppState
focusClass = focusFirst JA.classes J.EClass

focusMethod :: AppState -> AppState
focusMethod = focusFirst JA.methods J.EMethod

focusVariable :: AppState -> AppState
focusVariable = focusFirst JA.variables J.EField

focusUp :: AppState -> AppState
focusUp state = over focus F.focusUp state
