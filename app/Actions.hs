module Actions where

import Control.Lens
import Data.Tree

import qualified Java.Types as J
import qualified Java.Accessors as JA
import Java.Printer (printCommon)
import AppState (AppState, program, focus, output, running, lastResultTree, leafFocus)
import Output
import qualified Focus as F
import Commands.Types (ElementType(..))

readFocus :: AppState -> AppState
readFocus state =
  state & output .~ (Other $ putStrLn $ printCommon $ state ^.to leafFocus)

listSelectedElements :: [J.Element -> Bool] -> AppState -> AppState
listSelectedElements predicates state =
  state & output .~ (ResultTree $ JA.selectedElements predicates (state ^. program) (state ^.to leafFocus))

-- Focuses the first direct subelement of the current focus that satisfies the predicate
focusFirstOfSelectedElements :: (J.Element -> Bool) -> AppState -> AppState
focusFirstOfSelectedElements predicate state =
  let (Node _ elements) = JA.selectedElements [predicate] (state ^. program) (state ^.to leafFocus) in
    case elements of
      [] -> state & output .~ (Error $ putStrLn "No focusable element in scope")
      ((Node e _):_) -> state & focus %~ F.focusDown e

focusFirstSelectedElement :: String -> AppState -> AppState
focusFirstSelectedElement = focusFirstOfSelectedElements . JA.matchesTerm

focusFirstElement :: AppState -> AppState
focusFirstElement = focusFirstOfSelectedElements (pure True)

focusFirstElementOfType :: ElementType -> AppState -> AppState
focusFirstElementOfType = focusFirstOfSelectedElements . JA.matchesType

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
focusEndOfPath [] _ state = focusRoot state
focusEndOfPath [index] (Node _ elements) state =
  findOrElse index elements
    (\(Node label _) -> state & focus %~ F.focusDown label)
    (withIndexError state)
focusEndOfPath (index:restPath) (Node _ elements) state =
  findOrElse index elements
    (\e -> focusEndOfPath restPath e state)
    (withIndexError state)

readEndOfPath :: [Int] -> Tree J.Element -> AppState -> AppState
readEndOfPath [] _ state = readFocus state
readEndOfPath [index] (Node _ elements) state =
  findOrElse index elements
    (\(Node label _) -> state & output .~ (Other $ putStrLn $ printCommon label))
    (withIndexError state)
readEndOfPath (index:restPath) (Node _ elements) state =
  findOrElse index elements
    (\e -> readEndOfPath restPath e state)
    (withIndexError state)

withIndexError :: AppState -> AppState
withIndexError state = state & output .~ Error (putStrLn "The index does not exist in the last result tree")

-- Finds the element at the given index in the list and returns the transformation according to the given function
-- or returns the default if the index is out of bounds
findOrElse :: Int -> [a] -> (a -> b) -> b -> b
findOrElse index list f d =
  if index > 0 && index <= length list then
    f $ list !! (index - 1)
  else
    d

-- Causes the program to exit gracefully
exit :: AppState -> AppState
exit state = state
  & output .~ (Other $ putStr "Bye!")
  & running .~ False
