module Java.Printer
  ( printCommon
  , printPrompt
  , printMinimal
  ) where

import Java.Types

import Prelude hiding (Enum)
import Control.Lens
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Console.ANSI

printPrompt :: Element -> String
printPrompt e =
  printSignature e ++ case e of
    (EClass _) -> " { ... }"
    (EMethod _) -> " { ... }"
    _ -> ""
  
printProjectSignature :: Project -> String
printProjectSignature = view srcDir

printPackageSignature :: JavaFile -> String
printPackageSignature p = unwords
  [ "package"
  , p ^. packageName . idName -- TODO Fix this
  ]

printClassSignature :: Class -> String
printClassSignature c = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ c ^. classVisibility
  , if c ^. classFinal then Just "final" else Nothing
  , Just "class"
  , Just $ c ^. className . idName
  , ("extends "++) . view idName <$> c ^. classExtends
  , fmap ("implements "++) . notEmpty . intercalate ", " . fmap (view idName) $ c ^. classImplements
  ]
  where notEmpty "" = Nothing
        notEmpty str = Just str

printInterfaceSignature :: Interface -> String
printInterfaceSignature i = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ i ^. interfaceVisibility
  , Just "interface"
  , Just $ i ^. interfaceName . idName
  , fmap ("extends "++) $ notEmpty $ intercalate ", " $ view idName <$> i ^. interfaceExtends
  ]
  where notEmpty "" = Nothing
        notEmpty str = Just str

printEnumSignature :: Enum -> String
printEnumSignature e = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ e ^. enumVisibility
  , Just "enum"
  , Just $ e ^. enumName . idName
  ]

printMethodSignature :: Method -> String
printMethodSignature m = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ m ^. methodVisibility
  , if m ^. methodStatic then Just "static" else Nothing
  , Just $ m ^. methodReturnType . datatypeName
  , Just $ m ^. methodName . idName
  , Just $ "(" ++ intercalate ", " (printParameterSignature <$> m ^. methodParameters) ++ ")"
  ]

printConstructorSignature :: Constructor -> String
printConstructorSignature c = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ c ^. constructorVisibility
  , Just $ c ^. constructorName . idName
  , Just $ "(" ++ intercalate ", " (printParameterSignature <$> c ^. constructorParameters) ++ ")"
  ]

printParameterSignature :: Parameter -> String
printParameterSignature p = unwords
  [ p ^. parameterType . datatypeName
  , p ^. parameterName . idName
  ]

printFieldSignature :: Field -> String
printFieldSignature f = unwords $ catMaybes
  [ Just $ printVisibilityCommon $ f ^. fieldVisibility
  , if f ^. fieldStatic then Just "static" else Nothing
  , if f ^. fieldFinal then Just "final" else Nothing
  , Just $ f ^. fieldType . datatypeName
  , Just $ f ^. fieldName . idName
  ]

printSignature :: Element -> String
printSignature (EProject p) = printProjectSignature p
printSignature (EJavaFile p) = printPackageSignature p
printSignature (EClass c) = printClassSignature c
printSignature (EInterface i) = printInterfaceSignature i
printSignature (EEnum e) = printEnumSignature e
printSignature (EMethod m) = printMethodSignature m
printSignature (EConstructor c) = printConstructorSignature c
printSignature (EParameter p) = printParameterSignature p
printSignature (EField f) = printFieldSignature f
printSignature (EName n) = n ^. idName
printSignature (EType t) = t ^. datatypeName

printVisibilityCommon :: Visibility -> String
printVisibilityCommon Private = "private"
printVisibilityCommon Protected = "protected"
printVisibilityCommon Public = "public"

printProjectCommon :: Project -> String
printProjectCommon = view srcDir

printPackageCommon :: JavaFile -> String
printPackageCommon = view $ packageName . idName

printClassCommon :: Class -> String
printClassCommon c = unwords
  [ printClassSignature c
  , "{"
  , concat $ ("\n  "++) . (++";") . printFieldSignature <$> c ^. classFields
  , concat $ ("\n  "++) . printPrompt . EMethod <$> c ^. classMethods
  , "\n}"
  ]

printInterfaceCommon :: Interface -> String
printInterfaceCommon i = unwords
  [ printInterfaceSignature i
  , "{"
  , concat $ ("\n  "++) . printPrompt . EMethod <$> i ^. interfaceMethods
  , "\n}"
  ]

printEnumCommon :: Enum -> String
printEnumCommon e = unwords
  [ printEnumSignature e
  , "{"
  , concat $ ("\n  "++) . (++";") <$> e ^. enumConstants ^.. traverse.idName
  , concat $ ("\n  "++) . (++";") . printFieldSignature <$> e ^. enumFields
  , concat $ ("\n  "++) . printPrompt . EMethod <$> e ^. enumMethods
  , "\n}"
  ]

printMethodCommon :: Method -> String
printMethodCommon m = unwords
  [ printMethodSignature m
  , m ^. methodBody
  ]

printConstructorCommon :: Constructor -> String
printConstructorCommon c = unwords
  [ printConstructorSignature c
  , c ^. constructorBody
  ]

printParameterCommon :: Parameter -> String
printParameterCommon p = p ^. parameterName . idName

printFieldCommon :: Field -> String
printFieldCommon f = f ^. fieldName . idName

printCommon :: Element -> String
printCommon (EProject p) = printProjectCommon p
printCommon (EJavaFile p) = printPackageCommon p
printCommon (EClass c) = printClassCommon c
printCommon (EInterface i) = printInterfaceCommon i
printCommon (EEnum e) = printEnumCommon e
printCommon (EMethod m) = printMethodCommon m
printCommon (EConstructor c) = printConstructorCommon c
printCommon (EParameter p) = printParameterCommon p
printCommon (EField f) = printFieldCommon f
printCommon (EName n) = n ^. idName
printCommon (EType t) = t ^. datatypeName

-- TODO Duplicated in Output.hs
withSGR :: SGR -> IO () -> IO ()
withSGR sgr io = do
  setSGR [sgr]
  io
  setSGR [Reset]

colorize :: Color -> String -> IO ()
colorize color = withSGR (SetColor Foreground Vivid color) . putStr

keyword :: String -> IO ()
keyword "package" = colorize Blue "package"
keyword "class" = colorize Blue "class"
keyword "interface" = colorize Blue "interface"
keyword "enum" = colorize Blue "enum"
keyword "method" = colorize Green "method"
keyword "constructor" = colorize Magenta "constructor"
keyword "field" = colorize Cyan "field"
keyword "parameter" = colorize Yellow "parameter"
keyword _ = error ""

printMinimal :: Element -> IO ()
printMinimal (EProject p) = putStr "/"
printMinimal (EJavaFile p) = mconcat
  [ keyword "package"
  , putStr " "
  , putStr $ p ^. packageName . idName
  ]
printMinimal (EClass c) = mconcat
  [ keyword "class"
  , putStr " "
  , putStr $ c ^. className . idName
  ]
printMinimal (EInterface i) = mconcat
  [ keyword "interface"
  , putStr " "
  , putStr $ i ^. interfaceName . idName
  ]
printMinimal (EEnum e) = mconcat
  [ keyword "enum"
  , putStr " "
  , putStr $ e ^. enumName . idName
  ]
printMinimal (EMethod m) = mconcat
  [ keyword "method"
  , putStr " "
  , putStr $ m ^. methodName . idName
  , putStr $ "("
  , putStr $ intercalate "," $ m ^. methodParameters ^.. traverse.parameterType ^.. traverse.datatypeName
  , putStr $ "): "
  , putStr $ m ^. methodReturnType . datatypeName
  ]
printMinimal (EConstructor c) = mconcat
  [ keyword "constructor"
  , putStr " "
  , putStr $ c ^. constructorName . idName
  , putStr $ "("
  , putStr $ intercalate "," $ c ^. constructorParameters ^.. traverse.parameterType ^.. traverse.datatypeName
  , putStr $ ")"
  ]
printMinimal (EParameter p) = mconcat
  [ keyword "parameter"
  , putStr " "
  , putStr $ p ^. parameterName . idName
  ]
printMinimal (EField f) = mconcat
  [ keyword "field"
  , putStr " "
  , putStr $ f ^. fieldName . idName
  , putStr $ ": "
  , putStr $ f ^. fieldType . datatypeName
  ]
printMinimal (EName n) = putStr $ "name " ++ n ^. idName
printMinimal (EType t) = putStr $ "type " ++ t ^. datatypeName
