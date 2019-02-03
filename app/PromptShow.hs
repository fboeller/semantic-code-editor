module PromptShow where

import Java as J
import Control.Lens

printPackageSignature :: J.Package -> String
printPackageSignature p = p ^. J.packageName ^. idName

printClassSignature :: J.Class -> String
printClassSignature c = c ^. J.className ^. idName

printMethodSignature :: J.Method -> String
printMethodSignature m = m ^. J.methodName ^. idName

printParameterSignature :: J.Parameter -> String
printParameterSignature p = p ^. J.parameterName ^. idName

printFieldSignature :: J.Field -> String
printFieldSignature f = f ^. J.fieldName ^. idName

printSignature :: J.Element -> String
printSignature (J.EPackage p) = printPackageSignature p
printSignature (J.EClass c) = printClassSignature c
printSignature (J.EMethod m) = printMethodSignature m
printSignature (J.EParameter p) = printParameterSignature p
printSignature (J.EField f) = printFieldSignature f
