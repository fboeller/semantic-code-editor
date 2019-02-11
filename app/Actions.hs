module Actions where

import Control.Lens
import Data.Tree

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

listSelectedElements :: [J.Element -> Bool] -> AppState -> AppState
listSelectedElements predicates state =
  state & output .~ (ResultTree $ JA.selectedElements predicates $ state ^.to leafFocus)

focusFirst :: Tree J.Element -> AppState -> AppState
focusFirst (Node _ elements) state =
  case elements of
    [] -> state & output .~ (Error $ putStrLn "No focusable element in scope")
    ((Node e _):_) -> state & focus %~ (F.focusDown e)

focusFirstSelectedElementOfType :: ElementType -> String -> AppState -> AppState
focusFirstSelectedElementOfType elementType term state =
  focusFirst (JA.selectedElements [JA.allSatisfied [JA.matchesType elementType, JA.matchesTerm term]] $ state ^.to leafFocus) state

focusFirstSelectedElement :: String -> AppState -> AppState
focusFirstSelectedElement term state =
  focusFirst (JA.selectedElements [JA.matchesTerm term] $ state ^.to leafFocus) state

focusFirstElement :: AppState -> AppState
focusFirstElement state =
  focusFirst (JA.selectedElements [] $ state ^.to leafFocus) state

focusFirstElementOfType :: ElementType -> AppState -> AppState
focusFirstElementOfType elementType state =
  focusFirst (JA.selectedElements [JA.matchesType elementType] $ state ^.to leafFocus) state

focusUp :: AppState -> AppState
focusUp state = state & focus %~ F.focusUp

focusRoot :: AppState -> AppState
focusRoot state = state & focus %~ F.focusRoot

focusLastOutputByIndex :: Int -> AppState -> AppState
focusLastOutputByIndex index state =
  case state ^. lastOutput of
    ResultTree (Node _ elements) ->
      if index > 0 && index <= length elements then
        state & focus %~ (F.focusDown $ (\(Node label _) -> label) $ elements !! (index - 1))
      else
        state & output .~ (Error $ putStrLn $ "The index " ++ show index ++ " does not exist in the last result tree")
    _ -> state & output .~ (Error $ putStrLn $ "The last output was not a result list")
