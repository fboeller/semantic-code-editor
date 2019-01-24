{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import Lib
import qualified Language.Java.Parser as Java
import qualified Language.Java.Pretty as P
import Language.Java.Syntax ( CompilationUnit(..) )
import Text.Parsec.Error ( ParseError )

import Lens.Micro ( (.~), (^.), (&), set )
import Lens.Micro.TH ( makeLenses )
import Control.Monad ( void )
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.Main ( App(..), neverShowCursor, defaultMain, suspendAndResume, halt, continue )
import Brick.AttrMap ( attrMap )
import Brick.Types ( Widget, EventM, Next, BrickEvent(..) )
import Brick.Widgets.Core ( vBox, str )

programStr = "import java.util.*; public class MyClass { private int abc; public Integer doStuff(String p1) { /* Something */ return 4 + 6; }}"

data AppState =
  AppState { _program :: CompilationUnit
           }

makeLenses ''AppState

initialState :: AppState
initialState =
  AppState { _program = CompilationUnit Nothing [] []
           }

drawUI :: AppState -> [Widget ()]
drawUI state = [str $ processAST $ state ^. program]

compileProgram :: AppState -> EventM () (Next AppState)
compileProgram state =
  case Java.parser Java.compilationUnit programStr of
    Right ast -> continue $ set program ast state
    Left _ -> halt initialState

appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent state (VtyEvent e) =
    case e of
      V.EvKey V.KEsc [] -> halt state
      V.EvKey (V.KChar 'c') [] -> compileProgram state
      _ -> continue state
appEvent state _ = continue state

theApp :: App AppState e ()
theApp =
  App { appDraw = drawUI
      , appChooseCursor = neverShowCursor
      , appHandleEvent = appEvent
      , appStartEvent = return
      , appAttrMap = const $ attrMap V.defAttr []
      }

processAST :: CompilationUnit -> String
processAST ast =
  show $ P.pretty ast

main :: IO ()
main = do
  void $ defaultMain theApp initialState
