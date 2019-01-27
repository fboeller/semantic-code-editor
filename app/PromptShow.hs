module PromptShow where

import Java as J

import Control.Lens

class PromptShow a where
  printSignature :: a -> String

instance PromptShow J.Package where
  printSignature p = p ^. J.packageName ^. idName

instance PromptShow J.Class where
  printSignature c = c ^. J.className ^. idName

instance PromptShow J.Method where
  printSignature m = m ^. J.methodName ^. idName

instance PromptShow J.Parameter where
  printSignature p = p ^. J.parameterName ^. idName

instance PromptShow J.Field where
  printSignature f = f ^. J.fieldName ^. idName
