module PromptShow where

import Language.Java.Syntax
import Language.Java.Pretty ( pretty )

class PromptShow a where
  printSignature :: a -> String

instance PromptShow CompilationUnit where
  printSignature (CompilationUnit Nothing _ _) = "/"
  printSignature (CompilationUnit (Just packageDecl) _ _) = printSignature packageDecl

instance PromptShow PackageDecl where
  printSignature packageDecl = show $ pretty packageDecl

instance PromptShow TypeDecl where
  printSignature typeDecl = show $ pretty typeDecl

instance PromptShow ClassDecl where
  printSignature (ClassDecl modifiers (Ident ident) _ _ _ _) =
    unwords ((show <$> modifiers) ++ [ident])
  printSignature classDecl = show $ pretty classDecl
