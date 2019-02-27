module Java.Parser where

import qualified Java.Types as J
import Java.Converter

import Language.Java.Parser
import Language.Java.Syntax (CompilationUnit)
import Data.Traversable (traverse)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Exception
import Text.Parsec.Error (ParseError)
import System.Directory
import System.FilePath (takeExtension)
import System.Directory.Tree (AnchoredDirTree((:/)), DirTree(..), filterDir, readDirectoryWith)

data FileParseError = FileParseError FilePath String

runParserOnPath :: FilePath -> IO ([FileParseError], J.Project)
runParserOnPath path = do
    _ :/ tree <- readDirectoryWith return path
    dirtree <- traverse runParserOnFile $ filterDir myPred tree
    let maybeJavaFiles = foldr mappend ([], []) $ convertEither <$> dirtree -- TODO Not sure if the errors get concatenated
    return $ filesToProject <$> maybeJavaFiles
  where myPred (Dir ('.':_) _) = False
        myPred (File n _) = takeExtension n == ".java"
        myPred _ = True
        convertEither :: Either a b -> ([a], [b])
        convertEither (Left l) = ([l], [])
        convertEither (Right r) = ([], [r])

runParser :: String -> Either FileParseError J.JavaFile
runParser programStr = convertParseResult "" $ parser compilationUnit programStr

runParserOnFile :: FilePath -> IO (Either FileParseError J.JavaFile)
runParserOnFile file =
  (convertParseResult file . parser compilationUnit . T.unpack . TE.decodeUtf8 <$> BS.readFile file >>= evaluate)
  `catch` (\e -> return $ Left $ FileParseError file $ show (e :: IOException))

convertParseResult :: FilePath -> Either ParseError CompilationUnit -> Either FileParseError J.JavaFile
convertParseResult file (Left err) = Left $ FileParseError file $ show err
convertParseResult file (Right val) = Right $ convertCompilationUnit file val

filesToProject :: [J.JavaFile] -> J.Project
filesToProject javaFiles = J.Project { J._javaFiles = javaFiles }
