module JavaParser where

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty
import Data.Maybe (maybe, mapMaybe)
import Data.List (find)
import Control.Exception
import System.Directory
import System.FilePath
import Data.List (isSuffixOf)
import System.Posix.Files
import qualified Java as J
import Data.Traversable (traverse)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith
    )
import System.FilePath (takeExtension)

runParserOnPath :: FilePath -> IO (Maybe String, J.Project)
runParserOnPath path = do
    _ :/ tree <- readDirectoryWith return path
    dirtree <- traverse runParserOnFile $ filterDir myPred tree
    let maybeJavaFiles = foldr mappend (Nothing, []) $ convertEither <$> dirtree
    return $ filesToProject <$> maybeJavaFiles
  where myPred (Dir ('.':_) _) = False
        myPred (File n _) = takeExtension n == ".java"
        myPred _ = True
        convertEither :: Either String J.JavaFile -> (Maybe String, [J.JavaFile])
        convertEither (Left m) = (Just m, [])
        convertEither (Right javaFile) = (Nothing, [javaFile])

testProgram = "public class Dog { String breed; int age; String color; public void barking() { } private int hungry() { return 42; } protected void sleeping() { } }"

runParser :: String -> Either String J.JavaFile
runParser programStr = convertParseResult $ parser compilationUnit programStr

runParserOnFile :: FilePath -> IO (Either String J.JavaFile)
runParserOnFile file = (convertParseResult <$> parser compilationUnit <$> readFile file) `catch` (\e -> return $ Left $ show (e :: IOException))

convertParseResult (Left err) = Left $ show err
convertParseResult (Right val) = Right $ convertCompilationUnit val

filesToProject :: [J.JavaFile] -> J.Project
filesToProject javaFiles = J.Project { J._javaFiles = javaFiles }

convertCompilationUnit :: CompilationUnit -> J.JavaFile
convertCompilationUnit (CompilationUnit maybePackageDecl _ typeDecls) =
  J.JavaFile { J._packageName = convertMaybePackageDecl maybePackageDecl
             , J._classes = convertTypeDeclsToClasses typeDecls
             }

convertTypeDeclsToClasses :: [TypeDecl] -> [J.Class]
convertTypeDeclsToClasses = mapMaybe convertTypeDeclToClass

convertTypeDeclToClass :: TypeDecl -> Maybe J.Class
convertTypeDeclToClass (ClassTypeDecl classDecl) = convertClassDeclToClass classDecl
convertTypeDeclToClass (InterfaceTypeDecl interfaceDecl) = Nothing

convertClassDeclToClass :: ClassDecl -> Maybe J.Class
convertClassDeclToClass (ClassDecl modifiers (Ident name) _ _ _ classBody) = Just $
  J.Class { J._className = J.Identifier { J._idName = name }
          , J._classFields = convertClassBodyToFields classBody
          , J._classMethods = convertClassBodyToMethods classBody
          , J._classVisibility = convertModifiersToVisibility modifiers
          }
convertClassDeclToClass (EnumDecl _ _ _ _) = Nothing

convertClassBodyToMethods :: ClassBody -> [J.Method]
convertClassBodyToMethods (ClassBody decls) = mapMaybe convertDeclToMethod decls

convertDeclToMethod :: Decl -> Maybe J.Method
convertDeclToMethod (MemberDecl memberDecl) = convertMemberDeclToMethod memberDecl
convertDeclToMethod (InitDecl _ _) = Nothing

convertMemberDeclToMethod :: MemberDecl -> Maybe J.Method
convertMemberDeclToMethod (MethodDecl modifiers _ maybeT ident _ _ _ _) = Just $
  J.Method { J._methodName = J.Identifier { J._idName = prettyPrint ident }
           , J._methodParameters = []
           , J._methodReturnType = maybe (J.Datatype { J._datatypeName = "void" }) convertVarDeclToType maybeT
           , J._methodVisibility = convertModifiersToVisibility modifiers
           }
convertMemberDeclToMethod _ = Nothing

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
          }

convertVarDeclToType :: Type -> J.Datatype
convertVarDeclToType t =
  J.Datatype { J._datatypeName = prettyPrint t }

convertVarDeclToFieldName :: VarDecl -> String
convertVarDeclToFieldName (VarDecl (VarId (Ident name)) _) = name
convertVarDeclToFieldName _ = "No name out of array init yet" -- TODO

convertModifiersToVisibility :: [Modifier] -> J.Visibility
convertModifiersToVisibility modifiers =
  case find (`elem` [Public, Private, Protected]) modifiers of
    Nothing -> J.Private -- TODO Add package private here
    Just Public -> J.Public
    Just Private -> J.Private
    Just Protected -> J.Protected
    Just _ -> error "Non visibility modifier although filtered out"
    
    
convertMaybePackageDecl :: Maybe PackageDecl -> J.Identifier
convertMaybePackageDecl maybePackageDecl =
  maybe (J.Identifier { J._idName = "" }) convertPackageDecl maybePackageDecl

convertPackageDecl :: PackageDecl -> J.Identifier
convertPackageDecl (PackageDecl packageName) =
  J.Identifier { J._idName = prettyPrint packageName }
