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
read state =
  state & output .~ (Other $ putStrLn $ printCommon $ state ^.to leafFocus)

listAll :: AppState -> AppState
listAll state =
  state & output .~ (ResultTree $ JA.elementsRecursively $ state ^.to leafFocus)

listElementsOfType :: ElementType -> AppState -> AppState
listElementsOfType t state =
  state & output .~ (ResultList $ JA.elementsOfType t $ state ^.to leafFocus)

listSelectedElements :: String -> AppState -> AppState
listSelectedElements term state =
  state & output .~ (ResultList $ JA.selectedElements term $ state ^.to leafFocus)

listSelectedElementsOfType :: ElementType -> String -> AppState -> AppState
listSelectedElementsOfType elementType term state =
  state & output .~ (ResultList $ JA.selectedElementsOfType elementType term $ state ^.to leafFocus)

focusFirst :: (J.Element -> [a]) -> (a -> J.Element) -> AppState -> AppState
focusFirst subElements toElement state =
  case subElements (state ^.to leafFocus) of
    [] -> state & output .~ (Error $ putStrLn "No focusable element in scope")
    elements -> state & focus %~ changeFocus
      where
        changeFocus oldFocus = F.focusDown element oldFocus
        element = toElement $ head elements

focusFirstSelectedElementOfType :: ElementType -> String -> AppState -> AppState
focusFirstSelectedElementOfType elementType term =
  focusFirst (JA.selectedElementsOfType elementType term) id

focusFirstSelectedElement :: String -> AppState -> AppState
focusFirstSelectedElement term =
  focusFirst (JA.selectedElements term) id
    
focusAny :: AppState -> AppState
focusAny = focusFirst JA.elements id

focusClass :: AppState -> AppState
focusClass = focusFirst JA.classes J.EClass

focusMethod :: AppState -> AppState
focusMethod = focusFirst JA.methods J.EMethod

focusVariable :: AppState -> AppState
focusVariable = focusFirst JA.variables J.EField

focusUp :: AppState -> AppState
focusUp state = state & focus %~ F.focusUp

focusRoot :: AppState -> AppState
focusRoot state = state & focus %~ F.focusRoot

focusLastOutputByIndex :: Int -> AppState -> AppState
focusLastOutputByIndex index state =
  case state ^. lastOutput of
    ResultList elements ->
      if index > 0 && index <= length elements then
        state & focus %~ (F.focusDown $ elements !! (index - 1))
      else
        state & output .~ (Error $ putStrLn $ "The index " ++ show index ++ " does not exist in the last result list")
    _ -> state & output .~ (Error $ putStrLn $ "The last output was not a result list")
