module JavaAccessors where

import Control.Lens

import qualified Java as J

classes :: J.Element -> [J.Class]
classes (J.EPackage p) = p ^. J.classes
classes _ = []

selectedClasses :: String -> J.Element -> [J.Class]
selectedClasses term package =
  filter (\c -> c ^. J.className ^. J.idName == term) $ classes package

