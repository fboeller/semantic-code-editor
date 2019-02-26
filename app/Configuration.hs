{-# LANGUAGE TemplateHaskell #-}

module Configuration where

import Commands.Types (ParserType(..))

import Control.Lens

newtype Configuration =
  Configuration { _commandParserType :: ParserType
                }

makeLenses ''Configuration

initialConfig :: Configuration
initialConfig =
  Configuration { _commandParserType = Long
                }
