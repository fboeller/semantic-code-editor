module Java.Printer
  ( printCommon
  , printPrompt
  , printMinimal
  , printSignature
  , PrintMode(..)
  ) where

import Java.Types

import Prelude hiding (Enum, (<>))
import Control.Lens hiding (enum)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import System.Console.ANSI
import Text.PrettyPrint
import Language.Java.Pretty (pretty)

data PrintMode = Complete | Abbreviated

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

nested = nest 4

static = text "static"
final = text "final"
enum = text "enum"
package = text "package"
extends = text "extends"
implements = text "implements"
clazz = text "class"
interface = text "interface"

printPrompt :: Element -> IO ()
printPrompt = printMinimal

printProjectSignature :: Project -> Doc
printProjectSignature = text . view srcDir

printPackageSignature :: JavaFile -> Doc
printPackageSignature p = package <+> text (p ^. packageName . idName) -- TODO Fix this

printClassSignature :: Class -> Doc
printClassSignature c = hsep
  [ printVisibilityCommon $ c ^. classVisibility
  , if c ^. classFinal then final else empty
  , clazz
  , text $ c ^. className . idName
  , fromMaybe empty $ (extends <+>) . text . view idName <$> c ^. classExtends
  , if null $ c ^. classImplements then empty else implements <+> hsep (punctuate comma $ text . view idName <$> c ^. classImplements)
  ]
  
printInterfaceSignature :: Interface -> Doc
printInterfaceSignature i = hsep
  [ printVisibilityCommon $ i ^. interfaceVisibility
  , interface
  , text $ i ^. interfaceName . idName
  , if null $ i ^. interfaceExtends then empty else extends <+> hsep (punctuate comma $ text . view idName <$> i ^. interfaceExtends)
  ]

printEnumSignature :: Enum -> Doc
printEnumSignature e = hsep
  [ printVisibilityCommon $ e ^. enumVisibility
  , enum
  , text $ e ^. enumName . idName
  ]

printMethodSignature :: Method -> Doc
printMethodSignature m = hsep
  [ printVisibilityCommon $ m ^. methodVisibility
  , if m ^. methodStatic then static else empty
  , text $ m ^. methodReturnType . datatypeName
  , text $ m ^. methodName . idName
  , parens . hsep . punctuate comma $ printParameterSignature <$> m ^. methodParameters
  ]

printConstructorSignature :: Constructor -> Doc
printConstructorSignature c = hsep
  [ printVisibilityCommon $ c ^. constructorVisibility
  , text $ c ^. constructorName . idName
  , parens . hsep . punctuate comma $ printParameterSignature <$> c ^. constructorParameters
  ]

printParameterSignature :: Parameter -> Doc
printParameterSignature p = hsep
  [ text $ p ^. parameterType . datatypeName
  , text $ p ^. parameterName . idName
  ]

printFieldSignature :: Field -> Doc
printFieldSignature f = hsep
  [ printVisibilityCommon $ f ^. fieldVisibility
  , if f ^. fieldStatic then static else empty
  , if f ^. fieldFinal then final else empty
  , text $ f ^. fieldType . datatypeName
  , text $ f ^. fieldName . idName
  ]

printSignature :: Element -> String
printSignature = render . printSignature_

printSignature_ :: Element -> Doc
printSignature_ (EProject p) = printProjectSignature p
printSignature_ (EJavaFile p) = printPackageSignature p
printSignature_ (EClass c) = printClassSignature c
printSignature_ (EInterface i) = printInterfaceSignature i
printSignature_ (EEnum e) = printEnumSignature e
printSignature_ (EMethod m) = printMethodSignature m
printSignature_ (EConstructor c) = printConstructorSignature c
printSignature_ (EParameter p) = printParameterSignature p
printSignature_ (EField f) = printFieldSignature f
printSignature_ (EName n) = text $ n ^. idName
printSignature_ (EType t) = text $ t ^. datatypeName

printVisibilityCommon :: Visibility -> Doc
printVisibilityCommon Private = text "private"
printVisibilityCommon Protected = text "protected"
printVisibilityCommon Public = text "public"

printProjectCommon :: Project -> Doc
printProjectCommon = text . view srcDir

printJavaFileCommon :: PrintMode -> JavaFile -> Doc
printJavaFileCommon printMode file = vsep $ concat
  [ printClassCommon printMode <$> file ^. classes
  , printInterfaceCommon printMode <$> file ^. interfaces
  , printEnumCommon printMode <$> file ^. enums
  ]

printClassCommon :: PrintMode -> Class -> Doc
printClassCommon printMode c =
  withBraceBlock (printClassSignature c) (nested body)
  where
    body = vsep $ concat
      [ (<>semi) . printFieldSignature <$> c ^. classFields
      , printMethodCommon printMode <$> c ^. classMethods
      ]

printInterfaceCommon :: PrintMode -> Interface -> Doc
printInterfaceCommon printMode i =
  withBraceBlock (printInterfaceSignature i) (nested body)
  where
    body = vsep $ printMethodCommon printMode <$> i ^. interfaceMethods

printEnumCommon :: PrintMode -> Enum -> Doc
printEnumCommon printMode e =
  withBraceBlock (printEnumSignature e) (nested body)
  where
    body = vsep
      [ (hsep $ punctuate comma $ text <$> e ^. enumConstants ^.. traverse.idName) <> semi
      , vsep $ (<>semi) . printFieldSignature <$> e ^. enumFields
      , vsep $ printMethodCommon printMode <$> e ^. enumMethods
      ]

withBraceBlock :: Doc -> Doc -> Doc
withBraceBlock sig block = sig <+> lbrace $+$ block $+$ rbrace

printMethodCommon :: PrintMode -> Method -> Doc
printMethodCommon Complete m =
  case m ^. methodBody of
    Nothing -> printMethodSignature m <> semi
    Just body -> withBraceBlock (printMethodSignature m) (nested $ sep $ pretty <$> body)
printMethodCommon Abbreviated m = printMethodSignature m <+> text "{ ... }"

printConstructorCommon :: PrintMode -> Constructor -> Doc
printConstructorCommon Complete c =
  withBraceBlock (printConstructorSignature c) (nested $ sep $ pretty <$> c ^. constructorBody)
printConstructorCommon Abbreviated c = printConstructorSignature c <+> text "{ ... }"

printParameterCommon :: Parameter -> Doc
printParameterCommon p = text $ p ^. parameterName . idName

printFieldCommon :: Field -> Doc
printFieldCommon f = text $ f ^. fieldName . idName

printCommon :: PrintMode -> Element -> String
printCommon printMode = render . printCommon_ printMode

printCommon_ :: PrintMode -> Element -> Doc
printCommon_ _ (EProject p) = printProjectCommon p
printCommon_ printMode (EJavaFile p) = printJavaFileCommon printMode p
printCommon_ printMode (EClass c) = printClassCommon printMode c
printCommon_ printMode (EInterface i) = printInterfaceCommon printMode i
printCommon_ printMode (EEnum e) = printEnumCommon printMode e
printCommon_ printMode (EMethod m) = printMethodCommon printMode m
printCommon_ printMode (EConstructor c) = printConstructorCommon printMode c
printCommon_ _ (EParameter p) = printParameterCommon p
printCommon_ _ (EField f) = printFieldCommon f
printCommon_ _ (EName n) = text $ n ^. idName
printCommon_ _ (EType t) = text $ t ^. datatypeName

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
keyword "type" = colorize Magenta "type"
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
  , putStr $ ": "
  , putStr $ p ^. parameterType . datatypeName
  ]
printMinimal (EField f) = mconcat
  [ keyword "field"
  , putStr " "
  , putStr $ f ^. fieldName . idName
  , putStr $ ": "
  , putStr $ f ^. fieldType . datatypeName
  ]
printMinimal (EName n) = putStr $ "name " ++ n ^. idName
printMinimal (EType t) = mconcat
  [ keyword "type"
  , putStr " "
  , putStr $ t ^. datatypeName
  ]
