{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import qualified Language.Java.Parser as Java
import qualified Language.Java.Pretty as P
import Language.Java.Syntax ( CompilationUnit(..) )
import Text.Parsec.Error ( ParseError )

import Lens.Micro ( (.~), (^.), (&), set, Lens' )
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

/* Wrapper for a lens to prevent impredicative polymorphism */
data FocusLens a = FocusLens (Lens' CompilationUnit a)

data AppState a =
  AppState { _program :: CompilationUnit
           , _focus :: FocusLens a
           }

makeLenses ''AppState

initialState :: AppState CompilationUnit
initialState =
  AppState { _program = CompilationUnit Nothing [] []
           , _focus = FocusLens id
           }

drawUI :: AppState a -> [Widget ()]
drawUI state = [str $ processAST $ state ^. program]

compileProgram :: AppState a -> EventM () (Next (AppState a))
compileProgram state =
  case Java.parser Java.compilationUnit programStr of
    Right ast -> continue $ set program ast state
    Left _ -> halt state

appEvent :: AppState a -> BrickEvent () e -> EventM () (Next (AppState a))
appEvent state (VtyEvent e) =
    case e of
      V.EvKey V.KEsc [] -> halt state
      V.EvKey (V.KChar 'c') [] -> compileProgram state
      _ -> continue state
appEvent state _ = continue state

theApp :: App (AppState a) e ()
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
