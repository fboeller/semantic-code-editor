module Java.Printer
  ( printCommon
  , printSignature
  , printPrompt
  , printMinimal
  ) where

import Java
import Control.Lens
import Data.List (intercalate)
import Data.Maybe (catMaybes)

printPrompt :: Element -> String
printPrompt e =
  printSignature e ++ case e of
    (EClass _) -> " { ... }"
    (EMethod _) -> " { ... }"
    _ -> ""
  
printProjectSignature :: Project -> String
printProjectSignature p = "/"

printPackageSignature :: JavaFile -> String
printPackageSignature p = unwords
  [ "package"
  , p ^. packageName ^. idName -- TODO Fix this
  ]

printClassSignature :: Class -> String
printClassSignature c = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ c ^. classVisibility
  , if c ^. classFinal then Just "final" else Nothing
  , Just $ "class"
  , Just $ c ^. className ^. idName
  , fmap ("extends "++) $ fmap (view idName) $ c ^. classExtends
  , fmap ("implements "++) $ notEmpty $ intercalate ", " $ fmap (view idName) $ c ^. classImplements
  ]
  where notEmpty "" = Nothing
        notEmpty str = Just $ str

printInterfaceSignature :: Interface -> String
printInterfaceSignature i = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ i ^. interfaceVisibility
  , Just $ "interface"
  , Just $ i ^. interfaceName ^. idName
  , fmap ("extends "++) $ notEmpty $ intercalate ", " $ fmap (view idName) $ i ^. interfaceExtends
  ]
  where notEmpty "" = Nothing
        notEmpty str = Just $ str

printEnumSignature :: Java.Enum -> String
printEnumSignature e = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ e ^. enumVisibility
  , Just $ "enum"
  , Just $ e ^. enumName ^. idName
  ]

printMethodSignature :: Method -> String
printMethodSignature m = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ m ^. methodVisibility
  , if m ^. methodStatic then Just "static" else Nothing
  , Just $ m ^. methodReturnType ^. datatypeName
  , Just $ m ^. methodName ^. idName
  , Just $ "(" ++ (intercalate ", " $ printParameterSignature <$> m ^. methodParameters) ++ ")"
  ]

printParameterSignature :: Parameter -> String
printParameterSignature p = unwords
  [ p ^. parameterType ^. datatypeName
  , p ^. parameterName ^. idName
  ]

printFieldSignature :: Field -> String
printFieldSignature f = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ f ^. fieldVisibility
  , if f ^. fieldStatic then Just "static" else Nothing
  , if f ^. fieldFinal then Just "final" else Nothing
  , Just $ f ^. fieldType ^. datatypeName
  , Just $ f ^. fieldName ^. idName
  ]

printSignature :: Element -> String
printSignature (EProject p) = printProjectSignature p
printSignature (EJavaFile p) = printPackageSignature p
printSignature (EClass c) = printClassSignature c
printSignature (EInterface i) = printInterfaceSignature i
printSignature (EEnum e) = printEnumSignature e
printSignature (EMethod m) = printMethodSignature m
printSignature (EParameter p) = printParameterSignature p
printSignature (EField f) = printFieldSignature f
printSignature (EName n) = n ^. idName
printSignature (EType t) = t ^. datatypeName

printVisibilityCommon :: Visibility -> String
printVisibilityCommon Private = "private"
printVisibilityCommon Protected = "protected"
printVisibilityCommon Public = "public"

printProjectCommon :: Project -> String
printProjectCommon p = "/"

printPackageCommon :: JavaFile -> String
printPackageCommon p = p ^. packageName ^. idName

printClassCommon :: Class -> String
printClassCommon c = unwords
  [ printClassSignature c
  , "{"
  , concat $ ("\n  "++) <$> (++";") <$> printFieldSignature <$> c ^. classFields
  , concat $ ("\n  "++) <$> printPrompt <$> EMethod <$> c ^. classMethods
  , "\n}"
  ]

printInterfaceCommon :: Interface -> String
printInterfaceCommon i = unwords
  [ printInterfaceSignature i
  , "{"
  , concat $ ("\n  "++) <$> printPrompt <$> EMethod <$> i ^. interfaceMethods
  , "\n}"
  ]

printEnumCommon :: Java.Enum -> String
printEnumCommon e = unwords
  [ printEnumSignature e
  , "{"
  , concat $ ("\n  "++) <$> (++";") <$> e ^. enumConstants ^.. traverse.idName
  , concat $ ("\n  "++) <$> (++";") <$> printFieldSignature <$> e ^. enumFields
  , concat $ ("\n  "++) <$> printPrompt <$> EMethod <$> e ^. enumMethods
  , "\n}"
  ]

printMethodCommon :: Method -> String
printMethodCommon m = unwords $
  [ printMethodSignature m
  , m ^. methodBody
  ]

printParameterCommon :: Parameter -> String
printParameterCommon p = p ^. parameterName ^. idName

printFieldCommon :: Field -> String
printFieldCommon f = f ^. fieldName ^. idName

printCommon :: Element -> String
printCommon (EProject p) = printProjectCommon p
printCommon (EJavaFile p) = printPackageCommon p
printCommon (EClass c) = printClassCommon c
printCommon (EInterface i) = printInterfaceCommon i
printCommon (EEnum e) = printEnumCommon e
printCommon (EMethod m) = printMethodCommon m
printCommon (EParameter p) = printParameterCommon p
printCommon (EField f) = printFieldCommon f
printCommon (EName n) = n ^. idName
printCommon (EType t) = t ^. datatypeName

printMinimal :: Element -> String
printMinimal (EProject p) = "/"
printMinimal (EJavaFile p) = "package " ++ p ^. packageName ^. idName
printMinimal (EClass c) = "class " ++ c ^. className ^. idName
printMinimal (EInterface i) = "interface " ++ i ^. interfaceName ^. idName
printMinimal (EEnum e) = "enum " ++ e ^. enumName ^. idName
printMinimal (EMethod m) = concat
  [ m ^. methodName ^. idName
  , "("
  , intercalate "," $ m ^. methodParameters ^.. traverse.parameterType ^.. traverse.datatypeName
  , "): "
  , m ^. methodReturnType ^. datatypeName
  ]
printMinimal (EParameter p) = "parameter " ++ p ^. parameterName ^. idName
printMinimal (EField f) = f ^. fieldName ^. idName ++ ": " ++ f ^. fieldType ^. datatypeName
printMinimal (EName n) = "name " ++ n ^. idName
printMinimal (EType t) = "type " ++ t ^. datatypeName
