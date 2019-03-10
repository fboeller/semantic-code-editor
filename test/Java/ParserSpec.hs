module Java.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Containers
import Prelude hiding (Enum)
import Language.Java.Syntax (BlockStmt(..), Literal(..), Exp(..), Stmt(..))

import Java.Parser (runParser, FileParseError)
import Java.Printer (printCommon, printSignature, PrintMode(..))
import Java.Types

instance Arbitrary Identifier where
  arbitrary = do
    name <- ("a"++) <$> listOf1 (choose ('0', '9'))
    return $ Identifier { _idName = name }

instance Arbitrary Datatype where
  arbitrary = return $ Datatype { _datatypeName = "int" }

instance Arbitrary Visibility where
  arbitrary = oneof $ return <$> [Private, Protected, Public]

instance Arbitrary Field where
  arbitrary = do
    fieldName <- arbitrary
    fieldType <- arbitrary
    fieldVisibility <- arbitrary
    fieldStatic <- arbitrary
    fieldFinal <- arbitrary
    return $ Field
      { _fieldName = fieldName
      , _fieldType = fieldType
      , _fieldVisibility = fieldVisibility
      , _fieldStatic = fieldStatic
      , _fieldFinal = fieldFinal
      }

instance Arbitrary Parameter where
  arbitrary = do
    parameterName <- arbitrary
    parameterType <- arbitrary
    return $ Parameter
      { _parameterName = parameterName
      , _parameterType = parameterType
      }


instance Arbitrary Method where
  arbitrary = do
    methodName <- arbitrary
    methodParameters <- arbitrary
    methodReturnType <- arbitrary
    methodVisibility <- arbitrary
    methodStatic <- arbitrary
    return $ Method
      { _methodName = methodName
      , _methodParameters = methodParameters
      , _methodReturnType = methodReturnType
      , _methodVisibility = methodVisibility
      , _methodBody = Just [BlockStmt $ Return $ Just $ Lit $ Int 0]
      , _methodStatic = methodStatic
      }

newtype InterfaceMethod = InterfaceMethod { method :: Method }

instance Arbitrary InterfaceMethod where
  arbitrary = do
    methodName <- arbitrary
    methodParameters <- arbitrary
    methodReturnType <- arbitrary
    methodVisibility <- arbitrary
    methodStatic <- arbitrary
    return $ InterfaceMethod $ Method
      { _methodName = methodName
      , _methodParameters = methodParameters
      , _methodReturnType = methodReturnType
      , _methodVisibility = methodVisibility
      , _methodBody = Nothing
      , _methodStatic = methodStatic
      }
  
instance Arbitrary Constructor where
  arbitrary = do
    constructorName <- arbitrary
    constructorParameters <- arbitrary
    constructorVisibility <- arbitrary
    return $ Constructor
      { _constructorName = constructorName
      , _constructorParameters = constructorParameters
      , _constructorVisibility = constructorVisibility
      , _constructorBody = []
      }

instance Arbitrary Class where
  arbitrary = do
    className <- arbitrary
    classFields <- resize 10 $ listOf arbitrary
    classMethods <- resize 10 $ listOf arbitrary
    classVisibility <- arbitrary
    classExtends <- arbitrary
    classImplements <- resize 10 $ listOf arbitrary
    classFinal <- arbitrary
    return $ Class
      { _className = className
      , _classFields = classFields
      , _classMethods = classMethods
      , _classConstructors = [] -- TODO Put a choice of some valid constructors here
      , _classVisibility = classVisibility
      , _classExtends = classExtends
      , _classImplements = classImplements
      , _classFinal = classFinal
      }

instance Arbitrary Interface where
  arbitrary = do
    interfaceName <- arbitrary
    interfaceMethods <- resize 10 $ listOf arbitrary
    interfaceVisibility <- arbitrary
    interfaceExtends <- resize 10 $ listOf arbitrary
    return $ Interface
      { _interfaceName = interfaceName
      , _interfaceMethods = method <$> interfaceMethods
      , _interfaceVisibility = interfaceVisibility
      , _interfaceExtends = interfaceExtends
      }

instance Arbitrary Enum where
  arbitrary = do
    enumName <- arbitrary
    enumConstants <- resize 10 $ listOf1 arbitrary
    enumFields <- resize 10 $ listOf arbitrary
    enumMethods <- resize 10 $ listOf arbitrary
    enumVisibility <- arbitrary
    return $ Enum
      { _enumName = enumName
      , _enumConstants = enumConstants
      , _enumFields = enumFields
      , _enumMethods = enumMethods
      , _enumVisibility = enumVisibility
      }

instance Arbitrary JavaFile where
  arbitrary = do
    classes <- resize 10 $ listOf arbitrary
    interfaces <- resize 10 $ listOf arbitrary
    enums <- resize 10 $ listOf arbitrary
    return $ JavaFile
      { _fileName = ""
      , _packageName = Identifier { _idName = "" }
      , _classes = classes
      , _interfaces = interfaces
      , _enums = enums
      }
      
newtype PrettyJavaFile = PrettyJavaFile JavaFile deriving (Eq)
extractJavaFile (PrettyJavaFile e) = e

instance Show PrettyJavaFile where
  show = printCommon Complete . EJavaFile . extractJavaFile
  
instance Arbitrary PrettyJavaFile where
  arbitrary = PrettyJavaFile <$> arbitrary

spec = describe "Java parser" $ do
  
  it "defines an isomorphism with the printing into source code" $ property $
      \x -> (fmap PrettyJavaFile . runParser . printCommon Complete . EJavaFile . extractJavaFile) x === Right x
