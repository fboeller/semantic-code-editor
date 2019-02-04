module JavaAccessors where

import Control.Lens

import qualified Java as J

classes :: J.Element -> [J.Class]
classes (J.EPackage p) = p ^. J.classes
classes _ = []

selectedClasses :: String -> J.Element -> [J.Class]
selectedClasses term package =
  filter (\c -> c ^. J.className ^. J.idName == term) $ classes package

variables :: J.Element -> [J.Field]
variables (J.EClass c) = c ^. J.classFields
variables _ = []

methods :: J.Element -> [J.Method]
methods (J.EClass c) = c ^. J.classMethods
methods _ = []

elements :: J.Element -> [J.Element]
elements e = concat
  [ J.EClass <$> classes e
  , J.EField <$> variables e
  , J.EMethod <$> methods e
  ]
