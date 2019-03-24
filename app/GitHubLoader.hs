{-# LANGUAGE OverloadedStrings #-}

module GitHubLoader where

import qualified GitHub as GH
import GitHub.Endpoints.Repos.Contents
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import Data.Bifunctor

import Java.Parser (runParser, FileParseError)
import Java.Types (JavaFile, Project(..))

load :: String -> IO (Either String Project)
load path = do
    possibleFile <- contentsFor "iluwatar" "java-design-patterns" "flyweight/src/main/java/com/iluwatar/flyweight/Potion.java" Nothing
    return $ do
      content <- first show possibleFile
      first show $ toProject <$> toJavaFile content

toProject :: JavaFile -> Project
toProject javaFile = Project
  { _srcDir = "/"
  , _javaFiles = [javaFile]
  }


toJavaFile :: Content -> Either FileParseError JavaFile
toJavaFile = runParser . T.unpack . TE.decodeUtf8 . B64.decodeLenient . TE.encodeUtf8 . contentFileContent . fileOrError

fileOrError :: Content -> ContentFileData
fileOrError (ContentFile actualFile) = actualFile
fileOrError _ = error "Directories from GitHub can not be read yet"
