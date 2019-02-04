module JavaAccessors where

import Control.Lens hiding (elements)

import qualified Java as J
import CommandParser (SecondCommand(..))

selectedElements :: String -> J.Element -> [J.Element]
selectedElements term element =
  filter (matchesElement term) $ elements element

selectedElementsOfType :: SecondCommand -> String -> J.Element -> [J.Element]
selectedElementsOfType elementType term element =
  filter (matchesElement term) $ elementsOfType elementType element

matchesElement :: String -> J.Element -> Bool
matchesElement term (J.EClass c) = matchesClass term c
matchesElement term (J.EField c) = matchesField term c
matchesElement term (J.EMethod c) = matchesMethod term c
matchesElement _ _ = False

matchesClass :: String -> J.Class -> Bool
matchesClass term c = c ^. J.className ^. J.idName == term

matchesField :: String -> J.Field -> Bool
matchesField term c = c ^. J.fieldName ^. J.idName == term

matchesMethod :: String -> J.Method -> Bool
matchesMethod term c = c ^. J.methodName ^. J.idName == term

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
