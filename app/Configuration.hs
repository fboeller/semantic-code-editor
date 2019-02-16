{-# LANGUAGE TemplateHaskell #-}

module Configuration where

import Commands.Types (ParserType(..))

import Control.Lens

data Configuration =
  Configuration { _commandParserType :: ParserType
                }

makeLenses ''Configuration

initialConfig :: Configuration
initialConfig =
  Configuration { _commandParserType = Long
                }
