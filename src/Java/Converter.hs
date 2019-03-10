module Java.Converter(convertCompilationUnit) where

import qualified Java.Types as J
import qualified Java.Creators as JC

import Language.Java.Syntax
import Language.Java.Pretty (prettyPrint)
import Data.Maybe (maybe, mapMaybe)
import Data.List (find)

convertCompilationUnit :: FilePath -> CompilationUnit -> J.JavaFile
convertCompilationUnit path (CompilationUnit maybePackageDecl _ typeDecls) =
  J.JavaFile { J._fileName = path
             , J._packageName = maybePackageDeclToIdentifier maybePackageDecl
             , J._classes = mapMaybe toClass typeDecls
             , J._interfaces = mapMaybe toInterface typeDecls
             , J._enums = mapMaybe toEnum typeDecls
             }
  where
    toClass (ClassTypeDecl classDecl) = classDeclToClass classDecl
    toClass (InterfaceTypeDecl _) = Nothing
    toEnum (ClassTypeDecl classDecl) = classDeclToEnum classDecl
    toEnum (InterfaceTypeDecl _) = Nothing
    toInterface (ClassTypeDecl _) = Nothing
    toInterface (InterfaceTypeDecl interfaceDecl) = interfaceDeclToInterface interfaceDecl


maybePackageDeclToIdentifier :: Maybe PackageDecl -> J.Identifier
maybePackageDeclToIdentifier =
  maybe (JC.identifier "") packageDecl
  where
    packageDecl (PackageDecl packageName) = JC.identifier $ prettyPrint packageName

classDeclToClass :: ClassDecl -> Maybe J.Class
classDeclToClass EnumDecl{} = Nothing
classDeclToClass (ClassDecl modifiers (Ident name) _ maybeExtends implements (ClassBody decls)) = Just $
  J.Class { J._className = JC.identifier name
          , J._classFields = concatMap declToFields decls
          , J._classMethods = mapMaybe declToMethod decls
          , J._classConstructors = mapMaybe declToConstructor decls
          , J._classVisibility = toVisibility modifiers
          , J._classExtends = refTypeToIdentifier <$> maybeExtends
          , J._classImplements = refTypeToIdentifier <$> implements
          , J._classFinal = isFinal modifiers
          }

classDeclToEnum :: ClassDecl -> Maybe J.Enum
classDeclToEnum ClassDecl{} = Nothing
classDeclToEnum (EnumDecl modifiers (Ident name) _ (EnumBody constants decls)) = Just $
  J.Enum { J._enumName = JC.identifier name
         , J._enumConstants = toEnumConstant <$> constants
         , J._enumFields = concatMap declToFields decls
         , J._enumMethods = mapMaybe declToMethod decls
         , J._enumVisibility = toVisibility modifiers
         }
  where
    toEnumConstant (EnumConstant (Ident name) _ _) = JC.identifier name

interfaceDeclToInterface :: InterfaceDecl -> Maybe J.Interface
interfaceDeclToInterface (InterfaceDecl InterfaceAnnotation modifiers (Ident name) _ maybeExtends interfaceBody) = Nothing
interfaceDeclToInterface (InterfaceDecl InterfaceNormal modifiers (Ident name) _ maybeExtends (InterfaceBody decls)) = Just $
  J.Interface { J._interfaceName = JC.identifier name
              , J._interfaceMethods = mapMaybe memberDeclToMethod decls
              , J._interfaceVisibility = toVisibility modifiers
              , J._interfaceExtends = refTypeToIdentifier <$> maybeExtends
              }

refTypeToIdentifier :: RefType -> J.Identifier
refTypeToIdentifier = JC.identifier . prettyPrint

declToMethod :: Decl -> Maybe J.Method
declToMethod (MemberDecl memberDecl) = memberDeclToMethod memberDecl
declToMethod (InitDecl _ _) = Nothing

memberDeclToMethod :: MemberDecl -> Maybe J.Method
memberDeclToMethod (MethodDecl modifiers _ maybeT ident formalParams _ _ (MethodBody maybeBodyBlock)) = Just $
  J.Method { J._methodName = JC.identifier $ prettyPrint ident
           , J._methodParameters = toParameter <$> formalParams
           , J._methodReturnType = maybe (J.Datatype { J._datatypeName = "void" }) varDeclToType maybeT
           , J._methodVisibility = toVisibility modifiers
           , J._methodBody = (\(Block stmts) -> stmts) <$> maybeBodyBlock
           , J._methodStatic = isStatic modifiers
           }
memberDeclToMethod _ = Nothing

memberDeclToConstructor :: MemberDecl -> Maybe J.Constructor
memberDeclToConstructor (ConstructorDecl modifiers _ ident formalParams _ (ConstructorBody _ bodyBlock)) = Just $
  J.Constructor { J._constructorName = JC.identifier $ prettyPrint ident
                , J._constructorParameters = toParameter <$> formalParams
                , J._constructorVisibility = toVisibility modifiers
                , J._constructorBody = bodyBlock
                }
memberDeclToConstructor _ = Nothing

toParameter :: FormalParam -> J.Parameter
toParameter (FormalParam modifiers t _ varDeclId) =
  J.Parameter { J._parameterName = JC.identifier $ varDeclIdToFieldName varDeclId
              , J._parameterType = varDeclToType t
              }

declToConstructor :: Decl -> Maybe J.Constructor
declToConstructor (MemberDecl memberDecl) = memberDeclToConstructor memberDecl
declToConstructor (InitDecl _ _) = Nothing

declToFields :: Decl -> [J.Field]
declToFields (InitDecl _ _) = []
declToFields (MemberDecl (FieldDecl modifiers t varDecls)) =
  memberDeclToField modifiers t <$> varDecls
declToFields (MemberDecl _) = []

memberDeclToField :: [Modifier] -> Type -> VarDecl -> J.Field
memberDeclToField modifiers t (VarDecl varId _) =
  J.Field { J._fieldName = JC.identifier $ varDeclIdToFieldName varId
          , J._fieldType = varDeclToType t
          , J._fieldVisibility = toVisibility modifiers
          , J._fieldStatic = isStatic modifiers
          , J._fieldFinal = isFinal modifiers
          }

varDeclToType :: Type -> J.Datatype
varDeclToType t = J.Datatype { J._datatypeName = prettyPrint t }

varDeclIdToFieldName :: VarDeclId -> String
varDeclIdToFieldName (VarId (Ident name)) = name
varDeclIdToFieldName _ = "No name out of array init yet" -- TODO

toVisibility :: [Modifier] -> J.Visibility
toVisibility modifiers =
  case find (`elem` [Public, Private, Protected]) modifiers of
    Nothing -> J.Private -- TODO Add package private here
    Just Public -> J.Public
    Just Private -> J.Private
    Just Protected -> J.Protected
    Just _ -> error "Non visibility modifier although filtered out"    

isStatic :: [Modifier] -> Bool
isStatic = elem Static

isFinal :: [Modifier] -> Bool
isFinal = elem Final
