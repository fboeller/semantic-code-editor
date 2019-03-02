module Java.Accessors where

import Control.Lens hiding (elements)

import qualified Java.Types as J
import qualified Java.Creators as JC
import Commands.Types (ElementType(..))
import qualified Trees as T
import Data.List (isPrefixOf)
import Data.Tree (Tree, unfoldTree, flatten)
import Data.Char (toLower)
import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Control.Applicative (liftA2)

type SearchTerm = String

selectedElements :: [J.Element -> Bool] -> J.Project -> J.Element -> Tree J.Element
selectedElements predicates project =
  T.selectedBranches predicates . T.recursively (allElements project)

-- Returns if the elements matches the element type and search term
matchesAllGiven :: (Maybe ElementType, Maybe String) -> J.Element -> Bool
matchesAllGiven (maybeType, maybeTerm) = allSatisfied $ catMaybes
  [ matchesType <$> maybeType,
    matchesTerm <$> maybeTerm
  ]

-- Creates a function that returns True iff all given functions return True.
allSatisfied :: [a -> Bool] -> a -> Bool
allSatisfied = foldr (liftA2 (&&)) (pure True)

matchesTerm :: SearchTerm -> J.Element -> Bool
matchesTerm term element = isPrefixOf (toLower <$> term) $ toLower <$> searchProperty element

searchProperty :: J.Element -> String
searchProperty (J.EProject p) = ""
searchProperty (J.EJavaFile j) = j ^. J.fileName
searchProperty (J.EClass c) = c ^. J.className . J.idName
searchProperty (J.EInterface i) = i ^. J.interfaceName . J.idName
searchProperty (J.EEnum e) = e ^. J.enumName . J.idName
searchProperty (J.EField f) = f ^. J.fieldName . J.idName
searchProperty (J.EMethod m) = m ^. J.methodName . J.idName
searchProperty (J.EConstructor c) = c ^. J.constructorName . J.idName
searchProperty (J.EParameter p) = p ^. J.parameterName . J.idName
searchProperty (J.EName n) = n ^. J.idName
searchProperty (J.EType t) = t ^. J.datatypeName

matchesType :: ElementType -> J.Element -> Bool
matchesType Class (J.EClass _) = True
matchesType Interface (J.EInterface _) = True
matchesType Enum (J.EEnum _) = True
matchesType Variable (J.EField _) = True
matchesType Method (J.EMethod _) = True
matchesType Method (J.EConstructor _) = True
matchesType Parameter (J.EParameter _) = True
matchesType Extension (J.EClass _) = True
matchesType Name (J.EName _) = True
matchesType Type (J.EType _) = True
matchesType Definition (J.EClass _) = True
matchesType Definition (J.EInterface _) = True
matchesType Definition (J.EEnum _) = True
matchesType _ _ = False

-- All elements within an element that are of immediate interest
-- This elements aren't redundant, they are the essential elements defining the scope of the element 
standardElements :: J.Element -> [J.Element]
standardElements e = concat
  [ J.EClass <$> classes e
  , J.EInterface <$> interfaces e
  , J.EEnum <$> enums e
  , J.EConstructor <$> constructors e
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
allElements project e =
  standardElements e
  ++ definitions project e

classes :: J.Element -> [J.Class]
classes (J.EProject p) = J.EJavaFile <$> (p ^. J.javaFiles) >>= classes
classes (J.EJavaFile p) = p ^. J.classes
classes _ = []

interfaces :: J.Element -> [J.Interface]
interfaces (J.EProject p) = J.EJavaFile <$> (p ^. J.javaFiles) >>= interfaces
interfaces (J.EJavaFile p) = p ^. J.interfaces
interfaces _ = []

enums :: J.Element -> [J.Enum]
enums (J.EProject p) = J.EJavaFile <$> (p ^. J.javaFiles) >>= enums
enums (J.EJavaFile p) = p ^. J.enums
enums _ = []

variables :: J.Element -> [J.Field]
variables (J.EClass c) = c ^. J.classFields
variables (J.EEnum e) = e ^. J.enumFields
variables _ = []

parameters :: J.Element -> [J.Parameter]
parameters (J.EMethod p) = p ^. J.methodParameters
parameters (J.EConstructor p) = p ^. J.constructorParameters
parameters _ = []

methods :: J.Element -> [J.Method]
methods (J.EClass c) = c ^. J.classMethods
methods (J.EInterface i) = i ^. J.interfaceMethods
methods (J.EEnum e) = e ^. J.enumMethods
methods _ = []

constructors :: J.Element -> [J.Constructor]
constructors (J.EClass c) = c ^. J.classConstructors
constructors _ = []

extensions :: J.Element -> [J.Class]
extensions (J.EClass c) = maybeToList $ JC.emptyClass <$> (c ^. J.classExtends)
extensions _ = []

names :: J.Element -> [J.Identifier]
names (J.EClass c) = [c ^. J.className]
names (J.EInterface i) = [i ^. J.interfaceName]
names (J.EEnum e) = [e ^. J.enumName]
names (J.EField f) = [f ^. J.fieldName]
names (J.EMethod m) = [m ^. J.methodName]
names (J.EConstructor c) = [c ^. J.constructorName]
names (J.EParameter p) = [p ^. J.parameterName]
names _ = []

types :: J.Element -> [J.Datatype]
types (J.EField f) = [f ^. J.fieldType]
types (J.EMethod m) = [m ^. J.methodReturnType]
types (J.EParameter p) = [p ^. J.parameterType]
types _ = []

definitions :: J.Project -> J.Element -> [J.Element]
definitions project element = filter isOfType $ flatten $ T.recursively standardElements $ J.EProject project
  where
    isOfType :: J.Element -> Bool
    isOfType candidate = fromMaybe False $
      (==) <$> getTypeDefName candidate <*> getTypeUsageName element

getTypeUsageName :: J.Element -> Maybe String
getTypeUsageName (J.EField f) = Just $ f ^. J.fieldType . J.datatypeName
getTypeUsageName (J.EMethod m) = Just $ m ^. J.methodReturnType . J.datatypeName
getTypeUsageName (J.EParameter p) = Just $ p ^. J.parameterType . J.datatypeName
getTypeUsageName _ = Nothing

getTypeDefName :: J.Element -> Maybe String
getTypeDefName (J.EClass c) = Just $ c ^. J.className . J.idName
getTypeDefName (J.EInterface i) = Just $ i ^. J.interfaceName . J.idName
getTypeDefName (J.EEnum e) = Just $ e ^. J.enumName . J.idName
getTypeDefName _ = Nothing
