module PromptShow where

import Java
import Control.Lens
import Data.List (intercalate)

printPackageSignature :: JavaFile -> String
printPackageSignature p = unwords
  [ "package"
  , p ^. packageName ^. idName
  ]

printClassSignature :: Class -> String
printClassSignature c = unwords
  [ printVisibilityCommon $ c ^. classVisibility
  , "class"
  , c ^. className ^. idName
  ]

printMethodSignature :: Method -> String
printMethodSignature m = unwords
  [ printVisibilityCommon $ m ^. methodVisibility
  , m ^. methodReturnType ^. datatypeName
  , m ^. methodName ^. idName
  , "(" ++ (intercalate ", " $ printParameterSignature <$> m ^. methodParameters) ++ ")"
  ]

printParameterSignature :: Parameter -> String
printParameterSignature p = unwords
  [ p ^. parameterType ^. datatypeName
  , p ^. parameterName ^. idName
  ]

printFieldSignature :: Field -> String
printFieldSignature f = unwords
  [ printVisibilityCommon $ f ^. fieldVisibility
  , f ^. fieldType ^. datatypeName
  , f ^. fieldName ^. idName
  ]

printSignature :: Element -> String
printSignature (EJavaFile p) = printPackageSignature p
printSignature (EClass c) = printClassSignature c
printSignature (EMethod m) = printMethodSignature m
printSignature (EParameter p) = printParameterSignature p
printSignature (EField f) = printFieldSignature f

printVisibilityCommon :: Visibility -> String
printVisibilityCommon Private = "private"
printVisibilityCommon Protected = "protected"
printVisibilityCommon Public = "public"

printPackageCommon :: JavaFile -> String
printPackageCommon p = p ^. packageName ^. idName

printClassCommon :: Class -> String
printClassCommon c = unwords
  [ printClassSignature c
  , "{"
  , concat $ ("\n  "++) <$> (++";") <$> printFieldSignature <$> c ^. classFields
  , concat $ ("\n  "++) <$> printMethodCommon <$> c ^. classMethods
  , "\n}"
  ]

printMethodCommon :: Method -> String
printMethodCommon m = unwords $
  [ printMethodSignature m
  , "{"
  , "..."
  , "}"
  ]

printParameterCommon :: Parameter -> String
printParameterCommon p = p ^. parameterName ^. idName

printFieldCommon :: Field -> String
printFieldCommon f = f ^. fieldName ^. idName

printCommon :: Element -> String
printCommon (EJavaFile p) = printPackageCommon p
printCommon (EClass c) = printClassCommon c
printCommon (EMethod m) = printMethodCommon m
printCommon (EParameter p) = printParameterCommon p
printCommon (EField f) = printFieldCommon f


