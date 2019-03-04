module Actions where

import Control.Lens
import Data.Tree
import Data.Maybe (maybe)

import qualified Java.Types as J
import qualified Java.Accessors as JA
import Java.Printer (printCommon)
import AppState (AppState, project, focus, output, running, lastResultTree, leafFocus)
import Output
import qualified Focus as F
import qualified Commands.Types as P

import qualified Trees as T

readFocus :: AppState -> AppState
readFocus state =
  state & output .~ (Other $ putStrLn $ printCommon $ state ^.to leafFocus)

listSelectedElements :: [J.Element -> Bool] -> AppState -> AppState
listSelectedElements predicates state =
  state & output .~ (ResultTree $ JA.selectedElements predicates (state ^. project) (state ^.to leafFocus))

-- Focuses the first direct subelement of the current focus that satisfies the predicate
focusFirstOfSelectedElements :: (J.Element -> Bool) -> AppState -> AppState
focusFirstOfSelectedElements predicate state =
  let (Node _ elements) = JA.selectedElements [predicate] (state ^. project) (state ^.to leafFocus) in
    case elements of
      [] -> state & output .~ (Error $ putStrLn "No focusable element in scope")
      ((Node e _):_) -> state & focus %~ F.focusDown e

focusFirstElement :: AppState -> AppState
focusFirstElement = focusFirstOfSelectedElements (pure True)

-- Focuses the previously focused element
focusUp :: AppState -> AppState
focusUp = focus %~ F.focusUp

-- Focuses the top most element in the project
focusRoot :: AppState -> AppState
focusRoot = focus %~ F.focusRoot

-- Executes the given function if there is a last result tree
-- Otherwise, writes an error into the state
withLastResultTree :: (Tree J.Element -> AppState -> AppState) -> AppState -> AppState
withLastResultTree f state =
  case state ^. lastResultTree of
    Just tree -> f tree state
    Nothing -> state & output .~ Error (putStrLn "The last output was not a result tree")

-- Executes the given function if an element at the index can be found
-- Otherwise, writes an error into the state
withElementAtIndex :: (Tree J.Element -> AppState -> AppState) -> [Int] -> Tree J.Element -> AppState -> AppState
withElementAtIndex f path tree state =
  case T.elementAtIndex path tree of
    Just tree -> f tree state
    Nothing -> state & output .~ Error (putStrLn "The index does not exist in the last result tree")

onLastOutputByIndex :: P.FirstCommand -> [Int] -> AppState -> AppState
onLastOutputByIndex cmd = withLastResultTree . onEndOfPath cmd

onEndOfPath :: P.FirstCommand -> [Int] -> Tree J.Element -> AppState -> AppState
onEndOfPath P.Focus = focusEndOfPath
onEndOfPath P.Read = readEndOfPath
onEndOfPath P.List = listEndOfPath

focusEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
focusEndOfPath = withElementAtIndex $ over focus . F.focusDown . rootLabel

readEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
readEndOfPath = withElementAtIndex $ set output . Other . putStrLn . printCommon . rootLabel

listEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
listEndOfPath path tree state =
  (withElementAtIndex $ set output . ResultTree . JA.selectedElements [pure True] (state ^. project) . rootLabel) path tree state

-- Causes the program to exit gracefully
exit :: AppState -> AppState
exit state = state
  & output .~ (Other $ putStr "Bye!")
  & running .~ False
