module JavaAccessors where

import Control.Lens hiding (elements)

import qualified Java as J
import CommandParser (SecondCommand(..))
import Data.List (isPrefixOf)

selectedElements :: String -> J.Element -> [J.Element]
selectedElements term element =
  filter (matchesElement term) $ elements element

selectedElementsOfType :: SecondCommand -> String -> J.Element -> [J.Element]
selectedElementsOfType elementType term element =
  filter (matchesElement term) $ elementsOfType elementType element

matchesElement :: String -> J.Element -> Bool
matchesElement term (J.EClass c) = isPrefixOf term $ searchPropertiesOfClass c
matchesElement term (J.EField f) = isPrefixOf term $ searchPropertiesOfField f
matchesElement term (J.EMethod m) = isPrefixOf term $ searchPropertiesOfMethod m
matchesElement _ _ = False

searchPropertiesOfClass :: J.Class -> String
searchPropertiesOfClass c = c ^. J.className ^. J.idName

searchPropertiesOfField :: J.Field -> String
searchPropertiesOfField f = f ^. J.fieldName ^. J.idName

searchPropertiesOfMethod :: J.Method -> String
searchPropertiesOfMethod m = m ^. J.methodName ^. J.idName

classes :: J.Element -> [J.Class]
classes (J.EPackage p) = p ^. J.classes
classes _ = []

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

elementsOfType :: SecondCommand -> J.Element -> [J.Element]
elementsOfType Class = fmap J.EClass . classes
elementsOfType Variable = fmap J.EField . variables
elementsOfType Method = fmap J.EMethod . methods
elementsOfType _ = \_ -> []
