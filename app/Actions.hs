module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, lastOutput, leafFocus, Output(..))
import qualified Focus as F
import PromptShow

read :: AppState -> AppState
read state = set output (Other $ putStrLn $ printCommon $ state ^. to leafFocus) state

list :: (J.Element -> [a]) -> AppState -> [a]
list subElements state = subElements (state ^.to leafFocus)

listAll :: AppState -> AppState
listAll state = set output (ResultList results) state
  where
    results = concat
      [ J.EClass <$> list JA.classes state
      , J.EMethod <$> list JA.methods state
      , J.EField <$> list JA.variables state
      ]

listClasses :: AppState -> AppState
listClasses state = set output (ResultList $ J.EClass <$> list JA.classes state) state

listMethods :: AppState -> AppState
listMethods state = set output (ResultList $ J.EMethod <$> list JA.methods state) state

listVariables :: AppState -> AppState
listVariables state = set output (ResultList $ J.EField <$> list JA.variables state) state

listSelectedClasses :: String -> AppState -> AppState
listSelectedClasses term state = set output (ResultList $ J.EClass <$> JA.selectedClasses term (state ^.to leafFocus)) state

focusFirst :: (J.Element -> [a]) -> (a -> J.Element) -> AppState -> AppState
focusFirst subElements toElement state =
  case subElements (state ^.to leafFocus) of
    [] -> set output (Error $ putStrLn "No focusable element in scope") state
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

focusLastOutputByIndex :: Int -> AppState -> AppState
focusLastOutputByIndex index state =
  case state ^. lastOutput of
    ResultList elements ->
      if index > 0 && index <= length elements then
        over focus (F.focusDown $ elements !! (index - 1)) state
      else
        set output (Error $ putStrLn $ "The index " ++ show index ++ " does not exist in the last result list") state
    _ -> set output (Error $ putStrLn $ "The last output was not a result list") state
