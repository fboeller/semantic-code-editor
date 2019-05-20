module Java.Accessors where

import Control.Lens hiding (elements)

import qualified Java.Types as J
import qualified Java.Creators as JC
import Commands.Types (ElementType(..))
import qualified Trees as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Tree (Tree, unfoldTree, flatten)
import Data.Char (toLower)
import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Applicative (liftA2)

type SearchTerm = String

selectedElements :: [J.Element -> Bool] -> J.Project -> J.Element -> Tree J.Element
selectedElements predicates project =
  T.sortBy (comparing elementTypeOf <> comparing name)
  . T.selectedBranches predicates
  . T.recursively (allElements project)

-- Returns if the elements matches the element type and search term
matchesAllGiven :: (Maybe ElementType, Maybe String, Maybe String) -> J.Element -> Bool
matchesAllGiven (maybeElementType, maybeNameTerm, maybeTypeTerm) = allSatisfied $ catMaybes
  [ matchesElementType <$> maybeElementType,
    matchesTermAsInfix <$> maybeNameTerm,
    matchesTermAsInfixByType <$> maybeTypeTerm
  ]

-- Creates a function that returns True iff all given functions return True.
allSatisfied :: [a -> Bool] -> a -> Bool
allSatisfied = foldr (liftA2 (&&)) (pure True)

preprocess :: String -> String
preprocess = fmap toLower

matchesTermAsPrefix :: SearchTerm -> J.Element -> Bool
matchesTermAsPrefix term = matchesTerm isPrefixOf term . name

matchesTermAsInfix :: SearchTerm -> J.Element -> Bool
matchesTermAsInfix term = matchesTerm isInfixOf term . name

matchesTermAsInfixByType :: SearchTerm -> J.Element -> Bool
matchesTermAsInfixByType term element =
  fromMaybe False $ matchesTerm isInfixOf term <$> view J.datatypeName <$> typeOf element

matchesTerm :: (String -> String -> Bool) -> String -> String -> Bool
matchesTerm = (`on` preprocess)

matchesElementType :: ElementType -> J.Element -> Bool
matchesElementType Class (J.EClass _) = True
matchesElementType Interface (J.EInterface _) = True
matchesElementType Enum (J.EEnum _) = True
matchesElementType Field (J.EField _) = True
matchesElementType Method (J.EMethod _) = True
matchesElementType Method (J.EConstructor _) = True
matchesElementType Parameter (J.EParameter _) = True
matchesElementType Extension (J.EClass _) = True
matchesElementType Name (J.EName _) = True
matchesElementType Type (J.EType _) = True
matchesElementType Definition (J.EClass _) = True
matchesElementType Definition (J.EInterface _) = True
matchesElementType Definition (J.EEnum _) = True
matchesElementType _ _ = False

elementTypeOf :: J.Element -> ElementType
elementTypeOf (J.EClass _) = Class
elementTypeOf (J.EInterface _) = Interface
elementTypeOf (J.EEnum _) = Enum
elementTypeOf (J.EField _) = Field
elementTypeOf (J.EMethod _) = Method
elementTypeOf (J.EConstructor _) = Method
elementTypeOf (J.EParameter _) = Parameter
elementTypeOf (J.EName _) = Name
elementTypeOf (J.EType _) = Type
elementTypeOf (J.EJavaFile _) = error "Can't query the element type of a Java file"
elementTypeOf (J.EProject _) = error "Can't query the element type of a project"

-- All elements within an element that are of immediate interest
-- This elements aren't redundant, they are the essential elements defining the scope of the element 
standardElements :: J.Element -> [J.Element]
standardElements e = concat
  [ J.EClass <$> classes e
  , J.EInterface <$> interfaces e
  , J.EEnum <$> enums e
  , J.EConstructor <$> constructors e
  , J.EField <$> fields e
  , J.EMethod <$> methods e
  , J.EParameter <$> parameters e
  , maybeToList $ J.EType <$> typeOf e
  ]

-- All elements that describe properties of the element
-- These properties can be simply accessed and aren't the result of a complex analysis
extendedElements :: J.Element -> [J.Element]
extendedElements e = concat
  [ standardElements e
  , J.EClass <$> extensions e
  , pure $ J.EName $ J.Identifier $ name e
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

fields :: J.Element -> [J.Field]
fields (J.EClass c) = c ^. J.classFields
fields (J.EEnum e) = e ^. J.enumFields
fields _ = []

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
extensions (J.EClass c) = maybeToList $ JC.emptyClass <$> c ^. J.classExtends
extensions _ = []

name :: J.Element -> String
name (J.EProject p) = ""
name (J.EJavaFile j) = j ^. J.fileName
name (J.EClass c) = c ^. J.className . J.idName
name (J.EInterface i) = i ^. J.interfaceName . J.idName
name (J.EEnum e) = e ^. J.enumName . J.idName
name (J.EField f) = f ^. J.fieldName . J.idName
name (J.EMethod m) = m ^. J.methodName . J.idName
name (J.EConstructor c) = c ^. J.constructorName . J.idName
name (J.EParameter p) = p ^. J.parameterName . J.idName
name (J.EName n) = n ^. J.idName
name (J.EType t) = t ^. J.datatypeName

typeOf :: J.Element -> Maybe J.Datatype
typeOf (J.EField f) = Just $ f ^. J.fieldType
typeOf (J.EMethod m) = Just $ m ^. J.methodReturnType
typeOf (J.EParameter p) = Just $ p ^. J.parameterType
typeOf _ = Nothing

definitions :: J.Project -> J.Element -> [J.Element]
definitions project element = filter (isOfType element) $ flatten $ T.recursively standardElements $ J.EProject project

isOfType :: J.Element -> J.Element -> Bool
isOfType element candidate = fromMaybe False $
  (==) <$> getTypeDefName candidate <*> typeName element

typeName :: J.Element -> Maybe String
typeName (J.EType t) = Just $ t ^. J.datatypeName
typeName _ = Nothing

getTypeDefName :: J.Element -> Maybe String
getTypeDefName (J.EClass c) = Just $ c ^. J.className . J.idName
getTypeDefName (J.EInterface i) = Just $ i ^. J.interfaceName . J.idName
getTypeDefName (J.EEnum e) = Just $ e ^. J.enumName . J.idName
getTypeDefName _ = Nothing
