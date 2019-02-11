module PromptShow where

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
printCommon (EMethod m) = printMethodCommon m
printCommon (EParameter p) = printParameterCommon p
printCommon (EField f) = printFieldCommon f
printCommon (EName n) = n ^. idName
printCommon (EType t) = t ^. datatypeName


