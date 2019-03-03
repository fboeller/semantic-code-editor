module Commands.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.List (intercalate)
import Control.Lens ((^.))

import Commands.Parser
import Commands.Types
import Commands.LongParser (firstCommands, elementTypes)

firstCommandWords = elements $ (^.word) <$> firstCommands
elementTypeWords = elements $ (^.word) <$> elementTypes

spec = describe "runParser" $ do

  context "on empty input" $ do
  
    it "parses the empty string" $ do
      runParser Long "" `shouldBe` Right Empty
  
    it "parses only spaces" $ do
      runParser Long "  " `shouldBe` Right Empty

  context "with whitespace" $ do

    it "ignores leading spaces" $
      forAll firstCommandWords $
      \cmd -> runParser Long ("  " ++ cmd) `shouldSatisfy` isRight    

    it "ignores trailing spaces" $
      forAll firstCommandWords $
      \cmd -> runParser Long (cmd ++ "  ") `shouldSatisfy` isRight    

    it "ignores spaces between arguments of the list command" $
      forAll (listOf elementTypeWords) $
      \elementTypes -> runParser Long (intercalate "  " $ "list":elementTypes) `shouldSatisfy` isRight    

  context "on meta command" $ do

    describe "switch" $ do

      it "rejects without parameter" $ do
        runParser Long ":switch" `shouldSatisfy` isLeft

      it "parses with lowercase long parameter" $ do
        runParser Long ":switch long" `shouldBe` Right (Meta $ SwitchCommandParser Long)

      it "parses with uppercase long parameter" $ do
        pending
        runParser Long ":switch LONG" `shouldBe` Right (Meta $ SwitchCommandParser Long)

      it "parses with lowercase short parameter" $ do
        runParser Long ":switch short" `shouldBe` Right (Meta $ SwitchCommandParser Short)

      it "parses with uppercase short parameter" $ do
        pending
        runParser Long ":switch SHORT" `shouldBe` Right (Meta $ SwitchCommandParser Short)

    describe "load" $ do

      it "rejects without parameter" $ do
        runParser Long ":load" `shouldSatisfy` isLeft

      it "parses class name" $ do
        runParser Long ":load Example" `shouldBe` Right (Meta $ LoadFile "data/Example.java")

      it "parses java file path" $ do
        pending
        runParser Long ":load ../abc/Example.java" `shouldBe` Right (Meta $ LoadFile "../abc/Example.java")

      it "parses java file path as literal" $ do
        runParser Long ":load \"../abc/Example.java\"" `shouldBe` Right (Meta $ LoadFile "../abc/Example.java")

      it "parses directory" $ do
        pending
        runParser Long ":load /abc/example" `shouldBe` Right (Meta $ LoadFile "/abc/example")

      it "parses directory as literal" $ do
        runParser Long ":load \"/abc/example\"" `shouldBe` Right (Meta $ LoadFile "/abc/example")

  it "parses a quit command" $ do
    runParser Long "q" `shouldBe` Right Exit

  describe "list command" $ do

    it "parses without parameters" $ do
      runParser Long "list" `shouldBe` Right (Double List [])

    it "parses with single wildcard" $ do
      runParser Long "list *" `shouldBe` Right (Double List [(Nothing, Nothing)])

    it "parses with single element type" $ do
      runParser Long "list class" `shouldBe` Right (Double List [(Just Class, Nothing)])

    it "parses with single element type in brackets" $ do
      runParser Long "list (class)" `shouldBe` Right (Double List [(Just Class, Nothing)])

    it "parses with multiple element types" $ do
      runParser Long "list class method parameter"
        `shouldBe` Right (Double List [(Just Class, Nothing), (Just Method, Nothing), (Just Parameter, Nothing)])

    it "parses with single search term" $ do
      runParser Long "list \"abc\"" `shouldBe` Right (Double List [(Nothing, Just "abc")])

    it "parses with single search term in brackets" $ do
      runParser Long "list (\"abc\")" `shouldBe` Right (Double List [(Nothing, Just "abc")])

    it "parses with multiple search terms" $ do
      runParser Long "list \"abc\" \"QWE\" \"hjk\""
        `shouldBe` Right (Double List [(Nothing, Just "abc"), (Nothing, Just "QWE"), (Nothing, Just "hjk")])

    it "parses with single search term applied to element type" $ do
      runParser Long "list (method | \"abc\")" `shouldBe` Right (Double List [(Just Method, Just "abc")])

    it "parses with single search term applied to a wildcard" $ do
      pending
      runParser Long "list (* | \"abc\")" `shouldBe` Right (Double List [(Nothing, Just "abc")])

    it "parses with a complex mix" $ do
      runParser Long "list (class | \"abc\") * (method) \"hui\""
        `shouldBe` Right (Double List [(Just Class, Just "abc"), (Nothing, Nothing), (Just Method, Nothing), (Nothing, Just "hui")])

  describe "focus command" $ do

    it "parses without parameters" $ do
      runParser Long "focus" `shouldBe` Right (Double Focus [])

    it "parses with one digit number" $ do
      runParser Long "focus 1" `shouldBe` Right (IndexSingle Focus [1])

    it "parses with two digit number" $ do
      runParser Long "focus 12" `shouldBe` Right (IndexSingle Focus [12])

    it "parses with multiple number path" $ do
      runParser Long "focus 15.178.3" `shouldBe` Right (IndexSingle Focus [15, 178, 3])

    it "parses with single wildcard" $ do
      runParser Long "focus *" `shouldBe` Right (Double Focus [(Nothing, Nothing)])

    it "parses with single element type" $ do
      runParser Long "focus class" `shouldBe` Right (Double Focus [(Just Class, Nothing)])

    it "parses with single search term" $ do
      runParser Long "focus \"abc\"" `shouldBe` Right (Double Focus [(Nothing, Just "abc")])

  describe "read command" $ do

    it "parses without parameters" $ do
      runParser Long "read" `shouldBe` Right (Double Read [])
