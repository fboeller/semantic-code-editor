module Actions where

import Control.Lens

import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, lastOutput, leafFocus)
import Output
import qualified Focus as F
import CommandParser (ElementType(..))
import PromptShow

read :: AppState -> AppState
read state = set output (Other $ putStrLn $ printCommon $ state ^. to leafFocus) state

list :: (J.Element -> [a]) -> AppState -> [a]
list subElements state = subElements (state ^.to leafFocus)

listAll :: AppState -> AppState
listAll state = set output (ResultTree $ JA.elementsRecursively $ leafFocus state) state

listElementsOfType :: ElementType -> AppState -> AppState
listElementsOfType Class = listElementsWithYielder J.EClass JA.classes
listElementsOfType Variable = listElementsWithYielder J.EField JA.variables
listElementsOfType Method = listElementsWithYielder J.EMethod JA.methods
listElementsOfType Parameter = listElementsWithYielder J.EParameter JA.parameters
listElementsOfType Extension = listElementsWithYielder J.EClass JA.extensions
listElementsOfType Function = id

listElementsWithYielder :: (a -> J.Element) -> (J.Element -> [a]) -> AppState -> AppState
listElementsWithYielder f g state = set output (ResultList $ f <$> list g state) state

listSelectedElements :: String -> AppState -> AppState
listSelectedElements term state =
  state & output .~ (ResultList $ JA.selectedElements term (state ^.to leafFocus))

listSelectedElementsOfType :: ElementType -> String -> AppState -> AppState
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

focusFirstSelectedElementOfType :: ElementType -> String -> AppState -> AppState
focusFirstSelectedElementOfType elementType term = focusFirst (JA.selectedElementsOfType elementType term) id

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

focusRoot :: AppState -> AppState
focusRoot state = over focus F.focusRoot state

focusLastOutputByIndex :: Int -> AppState -> AppState
focusLastOutputByIndex index state =
  case state ^. lastOutput of
    ResultList elements ->
      if index > 0 && index <= length elements then
        over focus (F.focusDown $ elements !! (index - 1)) state
      else
        set output (Error $ putStrLn $ "The index " ++ show index ++ " does not exist in the last result list") state
    _ -> set output (Error $ putStrLn $ "The last output was not a result list") state
