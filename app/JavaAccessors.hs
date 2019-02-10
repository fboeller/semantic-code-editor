module JavaAccessors where

import Control.Lens hiding (elements)

import qualified Java as J
import CommandParser (ElementType(..))
import Data.List (isPrefixOf)
import Data.Tree
import Data.Char (toLower)
import Data.Maybe (maybeToList)

selectedElements :: String -> J.Element -> [J.Element]
selectedElements term element =
  filter (matchesElement term) $ elements element

selectedElementsOfType :: ElementType -> String -> J.Element -> [J.Element]
selectedElementsOfType elementType term element =
  filter (matchesElement term) $ elementsOfType elementType element

matchesElement :: String -> J.Element -> Bool
matchesElement term element = isPrefixOf (fmap toLower $ term) $ fmap toLower $
  case element of
    (J.EProject p) -> searchPropertiesOfProject p
    (J.EJavaFile j) -> searchPropertiesOfJavaFile j
    (J.EClass c) -> searchPropertiesOfClass c
    (J.EField f) -> searchPropertiesOfField f
    (J.EMethod m) -> searchPropertiesOfMethod m
    (J.EParameter p) -> searchPropertiesOfParameter p

searchPropertiesOfProject :: J.Project -> String
searchPropertiesOfProject p = ""

searchPropertiesOfJavaFile :: J.JavaFile -> String
searchPropertiesOfJavaFile j = j ^. J.fileName

searchPropertiesOfClass :: J.Class -> String
searchPropertiesOfClass c = c ^. J.className ^. J.idName

searchPropertiesOfField :: J.Field -> String
searchPropertiesOfField f = f ^. J.fieldName ^. J.idName

searchPropertiesOfMethod :: J.Method -> String
searchPropertiesOfMethod m = m ^. J.methodName ^. J.idName

searchPropertiesOfParameter :: J.Parameter -> String
searchPropertiesOfParameter p = p ^. J.parameterName ^. J.idName

classes :: J.Element -> [J.Class]
classes (J.EProject p) = J.EJavaFile <$> (p ^. J.javaFiles) >>= classes
classes (J.EJavaFile p) = p ^. J.classes
classes _ = []

variables :: J.Element -> [J.Field]
variables (J.EClass c) = c ^. J.classFields
variables _ = []

parameters :: J.Element -> [J.Parameter]
parameters (J.EMethod p) = p ^. J.methodParameters
parameters _ = []

methods :: J.Element -> [J.Method]
methods (J.EClass c) = c ^. J.classMethods
methods _ = []

extensions :: J.Element -> [J.Class]
extensions (J.EClass c) = maybeToList $ emptyClass <$> (c ^. J.classExtends)
extensions _ = []

elements :: J.Element -> [J.Element]
elements e = concat
  [ J.EClass <$> classes e
  , J.EField <$> variables e
  , J.EMethod <$> methods e
  , J.EParameter <$> parameters e
  , J.EClass <$> extensions e
  ]

treeprune :: Int -> Tree a -> Tree a
treeprune 0 t = Node (rootLabel t) []
treeprune d t = Node (rootLabel t) $ (treeprune $ d-1) <$> subForest t

elementsRecursivelyLimited :: Int -> J.Element -> Tree J.Element
elementsRecursivelyLimited limit = treeprune limit . elementsRecursively

elementsRecursively :: J.Element -> Tree J.Element
elementsRecursively = unfoldTree (\b -> (b, elements b))

elementsOfType :: ElementType -> J.Element -> [J.Element]
elementsOfType Class = fmap J.EClass . classes
elementsOfType Variable = fmap J.EField . variables
elementsOfType Method = fmap J.EMethod . methods
elementsOfType Parameter = fmap J.EParameter . parameters
elementsOfType Extension = fmap J.EClass . extensions
elementsOfType Function = \_ -> []

emptyClass :: J.Identifier -> J.Class
emptyClass identifier =
  J.Class { J._className = identifier
          , J._classFields = []
          , J._classMethods = []
          , J._classVisibility = J.Public
          , J._classExtends = Nothing
          , J._classImplements = []
          , J._classFinal = False
          }
