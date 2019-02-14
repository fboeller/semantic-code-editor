module JavaAccessors where

import Control.Lens hiding (elements)

import qualified Java as J
import CommandParser (ElementType(..))
import qualified Trees as T
import Data.List (isPrefixOf)
import Data.Tree (Tree, unfoldTree, flatten)
import Data.Char (toLower)
import Data.Maybe (maybeToList, maybe)
import Control.Applicative (liftA2)

type SearchTerm = String

selectedElements :: [J.Element -> Bool] -> J.Project -> J.Element -> Tree J.Element
selectedElements predicates project element =
  recursively (allElements project) element
  & T.levelFilteredTree predicates
  & T.treeprune (length predicates) 
  & T.cutEarlyLeafs (length predicates)

-- Creates a function that returns True iff all given functions return True.
allSatisfied :: [a -> Bool] -> a -> Bool
allSatisfied = foldr (liftA2 (&&)) (pure True)

matchesTerm :: SearchTerm -> J.Element -> Bool
matchesTerm term element = isPrefixOf (toLower <$> term) $ fmap toLower $
  case element of
    (J.EProject p) -> searchPropertiesOfProject p
    (J.EJavaFile j) -> searchPropertiesOfJavaFile j
    (J.EClass c) -> searchPropertiesOfClass c
    (J.EField f) -> searchPropertiesOfField f
    (J.EMethod m) -> searchPropertiesOfMethod m
    (J.EParameter p) -> searchPropertiesOfParameter p
    (J.EName n) -> n ^. J.idName
    (J.EType t) -> t ^. J.datatypeName

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

matchesType :: ElementType -> J.Element -> Bool
matchesType Class (J.EClass _) = True
matchesType Variable (J.EField _) = True
matchesType Method (J.EMethod _) = True
matchesType Parameter (J.EParameter _) = True
matchesType Extension (J.EClass _) = True
matchesType Name (J.EName _) = True
matchesType Type (J.EType _) = True
matchesType Definition (J.EClass _) = True
matchesType _ _ = False

-- All elements within an element that are of immediate interest
-- This elements aren't redundant, they are the essential elements defining the scope of the element 
standardElements :: J.Element -> [J.Element]
standardElements e = concat
  [ J.EClass <$> classes e
  , J.EField <$> variables e
  , J.EMethod <$> methods e
  , J.EParameter <$> parameters e
  ]

-- All elements that describe properties of the element
-- These properties can be simply accessed and aren't the result of a complex analysis
extendedElements :: J.Element -> [J.Element]
extendedElements e = concat
  [ standardElements e
  , J.EClass <$> extensions e
  , J.EName <$> names e
  , J.EType <$> types e
  ]

-- All elements of an element that are of
allElements :: J.Project -> J.Element -> [J.Element]
allElements project e = concat
  [ standardElements e
  , definitions project e
  ]

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

names :: J.Element -> [J.Identifier]
names (J.EClass c) = [c ^. J.className]
names (J.EField f) = [f ^. J.fieldName]
names (J.EMethod m) = [m ^. J.methodName]
names (J.EParameter p) = [p ^. J.parameterName]
names _ = []

types :: J.Element -> [J.Datatype]
types (J.EField f) = [f ^. J.fieldType]
types (J.EMethod m) = [m ^. J.methodReturnType]
types (J.EParameter p) = [p ^. J.parameterType]
types _ = []

definitions :: J.Project -> J.Element -> [J.Element]
definitions project element = filter isOfType $ flatten $ recursively standardElements $ J.EProject project
  where
    isOfType :: J.Element -> Bool
    isOfType candidate = maybe False id $
      (==) <$> getTypeDefName candidate <*> getTypeUsageName element

getTypeUsageName :: J.Element -> Maybe String
getTypeUsageName (J.EField f) = Just $ f ^. J.fieldType ^. J.datatypeName
getTypeUsageName (J.EMethod m) = Just $ m ^. J.methodReturnType ^. J.datatypeName
getTypeUsageName (J.EParameter p) = Just $ p ^. J.parameterType ^. J.datatypeName
getTypeUsageName _ = Nothing

getTypeDefName :: J.Element -> Maybe String
getTypeDefName (J.EClass c) = Just $ c ^. J.className ^. J.idName
getTypeDefName _ = Nothing

recursively :: (a -> [a]) -> a -> Tree a
recursively f = unfoldTree (\b -> (b, f b))

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
