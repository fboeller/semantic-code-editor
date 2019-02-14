module JavaParser where

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty
import Data.Maybe (maybe, mapMaybe)
import Data.List (find)
import Control.Exception
import System.Directory
import System.FilePath
import Text.Parsec.Error (ParseError)
import Data.List (isSuffixOf)
import System.Posix.Files
import qualified Java as J
import Data.Traversable (traverse)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith
    )
import System.FilePath (takeExtension)

data FileParseError = FileParseError FilePath String

runParserOnPath :: FilePath -> IO ([FileParseError], J.Project)
runParserOnPath path = do
    _ :/ tree <- readDirectoryWith return path
    dirtree <- traverse runParserOnFile $ filterDir myPred tree
    let maybeJavaFiles = foldr mappend ([], []) $ convertEither <$> dirtree -- TODO Not sure if the errors get concatenated
    return $ filesToProject <$> maybeJavaFiles
  where myPred (Dir ('.':_) _) = False
        myPred (File n _) = takeExtension n == ".java"
        myPred _ = True
        convertEither :: Either a b -> ([a], [b])
        convertEither (Left l) = ([l], [])
        convertEither (Right r) = ([], [r])

testProgram = "public class Dog { String breed; int age; String color; public void barking() { } private int hungry() { return 42; } protected void sleeping() { } }"

runParser :: String -> Either FileParseError J.JavaFile
runParser programStr = convertParseResult "" $ parser compilationUnit programStr

runParserOnFile :: FilePath -> IO (Either FileParseError J.JavaFile)
runParserOnFile file =
  (convertParseResult file <$> parser compilationUnit <$> readFile file)
  `catch` (\e -> return $ Left $ FileParseError file $ show (e :: IOException))

convertParseResult :: FilePath -> Either ParseError CompilationUnit -> Either FileParseError J.JavaFile
convertParseResult file (Left err) = Left $ FileParseError file $ show err
convertParseResult file (Right val) = Right $ convertCompilationUnit file val

filesToProject :: [J.JavaFile] -> J.Project
filesToProject javaFiles = J.Project { J._javaFiles = javaFiles }

convertCompilationUnit :: FilePath -> CompilationUnit -> J.JavaFile
convertCompilationUnit path (CompilationUnit maybePackageDecl _ typeDecls) =
  J.JavaFile { J._fileName = path
             , J._packageName = convertMaybePackageDecl maybePackageDecl
             , J._classes = mapMaybe convertTypeDeclToClass typeDecls
             , J._interfaces = mapMaybe convertTypeDeclToInterface typeDecls
             }

convertTypeDeclToClass :: TypeDecl -> Maybe J.Class
convertTypeDeclToClass (ClassTypeDecl classDecl) = convertClassDeclToClass classDecl
convertTypeDeclToClass _ = Nothing

convertTypeDeclToInterface :: TypeDecl -> Maybe J.Interface
convertTypeDeclToInterface (InterfaceTypeDecl interfaceDecl) = convertInterfaceDeclToInterface interfaceDecl
convertTypeDeclToInterface _ = Nothing

convertInterfaceDeclToInterface :: InterfaceDecl -> Maybe J.Interface
convertInterfaceDeclToInterface (InterfaceDecl InterfaceNormal modifiers (Ident name) _ maybeExtends interfaceBody) = Just $
  J.Interface { J._interfaceName = J.Identifier { J._idName = name }
              , J._interfaceMethods = convertInterfaceBodyToMethods interfaceBody
              , J._interfaceVisibility = convertModifiersToVisibility modifiers
              , J._interfaceExtends = convertRefTypeToIdentifier <$> maybeExtends
              }
convertInterfaceDeclToInterface _ = Nothing

convertInterfaceBodyToMethods :: InterfaceBody -> [J.Method]
convertInterfaceBodyToMethods (InterfaceBody decls) = mapMaybe convertMemberDeclToMethod decls

convertClassDeclToClass :: ClassDecl -> Maybe J.Class
convertClassDeclToClass (ClassDecl modifiers (Ident name) _ maybeExtends implements classBody) = Just $
  J.Class { J._className = J.Identifier { J._idName = name }
          , J._classFields = convertClassBodyToFields classBody
          , J._classMethods = convertClassBodyToMethods classBody
          , J._classVisibility = convertModifiersToVisibility modifiers
          , J._classExtends = convertRefTypeToIdentifier <$> maybeExtends
          , J._classImplements = convertRefTypeToIdentifier <$> implements
          , J._classFinal = isFinal modifiers
          }
convertClassDeclToClass (EnumDecl _ _ _ _) = Nothing

convertRefTypeToIdentifier :: RefType -> J.Identifier
convertRefTypeToIdentifier refType = J.Identifier { J._idName = prettyPrint refType }

convertClassBodyToMethods :: ClassBody -> [J.Method]
convertClassBodyToMethods (ClassBody decls) = mapMaybe convertDeclToMethod decls

convertDeclToMethod :: Decl -> Maybe J.Method
convertDeclToMethod (MemberDecl memberDecl) = convertMemberDeclToMethod memberDecl
convertDeclToMethod (InitDecl _ _) = Nothing

convertMemberDeclToMethod :: MemberDecl -> Maybe J.Method
convertMemberDeclToMethod (MethodDecl modifiers _ maybeT ident formalParams _ _ body) = Just $
  J.Method { J._methodName = J.Identifier { J._idName = prettyPrint ident }
           , J._methodParameters = convertFormalParam <$> formalParams
           , J._methodReturnType = maybe (J.Datatype { J._datatypeName = "void" }) convertVarDeclToType maybeT
           , J._methodVisibility = convertModifiersToVisibility modifiers
           , J._methodBody = prettyPrint body
           , J._methodStatic = isStatic modifiers
           }
convertMemberDeclToMethod _ = Nothing

convertFormalParam :: FormalParam -> J.Parameter
convertFormalParam (FormalParam modifiers t _ varDeclId) =
  J.Parameter { J._parameterName = J.Identifier { J._idName = convertVarDeclIdToFieldName varDeclId }
              , J._parameterType = convertVarDeclToType t
              }

convertMemberDeclToMethods _ = []

convertClassBodyToFields :: ClassBody -> [J.Field]
convertClassBodyToFields (ClassBody decls) = concatMap convertDeclToFields decls

convertDeclToFields :: Decl -> [J.Field]
convertDeclToFields (MemberDecl memberDecl) = convertMemberDeclToFields memberDecl
convertDeclToFields (InitDecl _ _) = []

convertMemberDeclToFields :: MemberDecl -> [J.Field]
convertMemberDeclToFields (FieldDecl modifiers t varDecls) =
  map (convertMemberDeclToField modifiers t) varDecls
convertMemberDeclToFields _ = []

convertMemberDeclToField :: [Modifier] -> Type -> VarDecl -> J.Field
convertMemberDeclToField modifiers t varDecl =
  J.Field { J._fieldName = J.Identifier { J._idName = convertVarDeclToFieldName varDecl }
          , J._fieldType = convertVarDeclToType t
          , J._fieldVisibility = convertModifiersToVisibility modifiers
          , J._fieldStatic = isStatic modifiers
          , J._fieldFinal = isFinal modifiers
          }

convertVarDeclToType :: Type -> J.Datatype
convertVarDeclToType t =
  J.Datatype { J._datatypeName = prettyPrint t }

convertVarDeclToFieldName :: VarDecl -> String
convertVarDeclToFieldName (VarDecl varId _) = convertVarDeclIdToFieldName varId

convertVarDeclIdToFieldName :: VarDeclId -> String
convertVarDeclIdToFieldName (VarId (Ident name)) = name
convertVarDeclIdToFieldName _ = "No name out of array init yet" -- TODO

convertModifiersToVisibility :: [Modifier] -> J.Visibility
convertModifiersToVisibility modifiers =
  case find (`elem` [Public, Private, Protected]) modifiers of
    Nothing -> J.Private -- TODO Add package private here
    Just Public -> J.Public
    Just Private -> J.Private
    Just Protected -> J.Protected
    Just _ -> error "Non visibility modifier although filtered out"    

isStatic :: [Modifier] -> Bool
isStatic = any (==Static)

isFinal :: [Modifier] -> Bool
isFinal = any (==Final)

convertMaybePackageDecl :: Maybe PackageDecl -> J.Identifier
convertMaybePackageDecl maybePackageDecl =
  maybe (J.Identifier { J._idName = "" }) convertPackageDecl maybePackageDecl

convertPackageDecl :: PackageDecl -> J.Identifier
convertPackageDecl (PackageDecl packageName) =
  J.Identifier { J._idName = prettyPrint packageName }
