module Actions where

import Control.Lens
import Data.Tree
import Data.Maybe (maybe)

import qualified Java.Types as J
import qualified Java.Accessors as JA
import Java.Printer (printCommon)
import AppState (AppState, program, focus, output, running, lastResultTree, leafFocus)
import Output
import qualified Focus as F
import Commands.Types (ElementType(..))

import qualified Trees as T

readFocus :: AppState -> AppState
readFocus state =
  state & output .~ (Other $ putStrLn $ printCommon $ state ^.to leafFocus)

listSelectedElements :: [J.Element -> Bool] -> AppState -> AppState
listSelectedElements predicates state =
  state & output .~ (ResultTree $ JA.selectedElements predicates (state ^. program) (state ^.to leafFocus))

listElementsOfIndex :: [Int] -> AppState -> AppState
listElementsOfIndex _ = id

-- Focuses the first direct subelement of the current focus that satisfies the predicate
focusFirstOfSelectedElements :: (J.Element -> Bool) -> AppState -> AppState
focusFirstOfSelectedElements predicate state =
  let (Node _ elements) = JA.selectedElements [predicate] (state ^. program) (state ^.to leafFocus) in
    case elements of
      [] -> state & output .~ (Error $ putStrLn "No focusable element in scope")
      ((Node e _):_) -> state & focus %~ F.focusDown e

focusFirstElement :: AppState -> AppState
focusFirstElement = focusFirstOfSelectedElements (pure True)

-- Focuses the previously focused element
focusUp :: AppState -> AppState
focusUp state = state & focus %~ F.focusUp

-- Focuses the top most element in the project
focusRoot :: AppState -> AppState
focusRoot state = state & focus %~ F.focusRoot

focusLastOutputByIndex :: [Int] -> AppState -> AppState
focusLastOutputByIndex indexPath state =
  case state ^. lastResultTree of
    Just tree -> focusEndOfPath indexPath tree state
    _ -> state & output .~ Error (putStrLn "The last output was not a result tree")

readLastOutputByIndex :: [Int] -> AppState -> AppState
readLastOutputByIndex indexPath state =
  case state ^. lastResultTree of
    Just tree -> readEndOfPath indexPath tree state
    _ -> state & output .~ Error (putStrLn "The last output was not a result tree")

focusEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
focusEndOfPath path tree state = maybe
  (withIndexError state)
  ((\e -> state & focus %~ e) . F.focusDown . rootLabel)
  (T.elementAtIndex path tree)

readEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
readEndOfPath path tree state = maybe
  (withIndexError state)
  ((\e -> state & output .~ e) . Other . putStrLn . printCommon . rootLabel)
  (T.elementAtIndex path tree)

withIndexError :: AppState -> AppState
withIndexError state = state & output .~ Error (putStrLn "The index does not exist in the last result tree")

-- Causes the program to exit gracefully
exit :: AppState -> AppState
exit state = state
  & output .~ (Other $ putStr "Bye!")
  & running .~ False
