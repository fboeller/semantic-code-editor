module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, lastOutput, leafFocus, Output(..))
import qualified Focus as F
import CommandParser (SecondCommand(..))
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

listElementsOfType :: SecondCommand -> AppState -> AppState
listElementsOfType Class = listClasses
listElementsOfType Method = listMethods
listElementsOfType Variable = listVariables
listElementsOfType _ = id

listClasses :: AppState -> AppState
listClasses state = set output (ResultList $ J.EClass <$> list JA.classes state) state

listMethods :: AppState -> AppState
listMethods state = set output (ResultList $ J.EMethod <$> list JA.methods state) state

listVariables :: AppState -> AppState
listVariables state = set output (ResultList $ J.EField <$> list JA.variables state) state

listSelectedElements :: String -> AppState -> AppState
listSelectedElements term state =
  state & output .~ (ResultList $ JA.selectedElements term (state ^.to leafFocus))

listSelectedElementsOfType :: SecondCommand -> String -> AppState -> AppState
listSelectedElementsOfType elementType term state =
  state & output .~ (ResultList $ JA.selectedElementsOfType elementType term (state ^.to leafFocus))

focusFirst :: (J.Element -> [a]) -> (a -> J.Element) -> AppState -> AppState
focusFirst subElements toElement state =
  case subElements (state ^.to leafFocus) of
    [] -> set output (Error $ putStrLn "No focusable element in scope") state
    elements -> over focus changeFocus state
      where
        changeFocus oldFocus = F.focusDown element oldFocus
        element = toElement $ head elements

focusFirstSelectedElement :: String -> AppState -> AppState
focusFirstSelectedElement term = focusFirst (JA.selectedElements term) id
    
focusAny :: AppState -> AppState
focusAny = focusFirst JA.elements id

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
