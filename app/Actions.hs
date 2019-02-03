module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, leafFocus)
import qualified Focus as F
import PromptShow

read :: AppState -> AppState
read state = set output (putStrLn $ printCommon $ state ^. to leafFocus) state

list :: (J.Element -> [a]) -> (a -> String) -> AppState -> IO ()
list subElements toString state = putStr $ unlines $ toString <$> subElements (state ^.to leafFocus)

listAll :: AppState -> AppState
listAll state = set output results state
  where
    results = do
      list JA.classes printClassSignature state
      list JA.methods printMethodSignature state
      list JA.variables printFieldSignature state

listClasses :: AppState -> AppState
listClasses state = set output (list JA.classes printClassSignature state) state

listMethods :: AppState -> AppState
listMethods state = set output (list JA.methods printMethodSignature state) state

listVariables :: AppState -> AppState
listVariables state = set output (list JA.variables printFieldSignature state) state

listSelectedClasses :: String -> AppState -> AppState
listSelectedClasses term state = set output (putStrLn $ unlines $ printClassSignature <$> JA.selectedClasses term (state ^.to leafFocus)) state

focusFirst :: (J.Element -> [a]) -> (a -> J.Element) -> AppState -> AppState
focusFirst subElements toElement state =
  case subElements (state ^.to leafFocus) of
    [] -> set output (putStrLn "No focusable element in scope") state
    elements -> over focus changeFocus state
      where
        changeFocus oldFocus = F.focusDown element oldFocus
        element = toElement $ head elements
    

focusClass :: AppState -> AppState
focusClass = focusFirst JA.classes J.EClass

focusMethod :: AppState -> AppState
focusMethod = focusFirst JA.methods J.EMethod

focusVariable :: AppState -> AppState
focusVariable = focusFirst JA.variables J.EField

focusUp :: AppState -> AppState
focusUp state = over focus F.focusUp state
