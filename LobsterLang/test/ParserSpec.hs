{-
-- EPITECH PROJECT, 2023
-- ParserSpec.hs
-- File description:
-- ParserSpec
-}

module ParserSpec where

import Test.Hspec
import Parse
import Parse ()
import qualified AST
import AST (Ast(Value))

spec :: Spec
spec = do
    describe "ParserTest" $ do
        it "Check parseChar Success" $ do
            runParser (parseChar ' ') (0,0) " Hello" `shouldBe` Right (' ', "Hello", (0,1))
        it "Check parseChar Failure" $ do
            runParser (parseChar ' ') (0,0) "Hello" `shouldBe` Left (errorParsing (0, 0))
        it "Check parseOr Success first arg" $ do
            runParser (parseOr (parseChar 'a') (parseChar ' ')) (0,0) "aHello" `shouldBe` Right ('a', "Hello", (0,1))
        it "Check parseOr Success second arg" $ do
            runParser (parseOr (parseChar 'a') (parseChar ' ')) (0,0) " Hello" `shouldBe` Right (' ', "Hello", (0,1))
        it "Check parseOr Failure" $ do
            runParser (parseOr (parseChar 'f') (parseChar 'O')) (0,0) " Oui" `shouldBe` Left (errorParsing (0,0))
        it "Check parseAnd Success" $ do
            runParser (parseAnd (parseChar 'a') (parseChar 'p')) (0,0) "apHello" `shouldBe` Right (('a', 'p'), "Hello", (0,2))
        it "Check parseAnd Failure" $ do
            runParser (parseAnd (parseChar 'e') (parseChar 'p')) (0,0) "apHello" `shouldBe` Left (errorParsing (0,0))
        it "Check parseAndWith Number Success" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['0'..'9']) (parseAnyChar ['0'..'9'])) (0,0) "42Hello" `shouldBe` Right ("42", "Hello", (0, 2))
        it "Check parseAndWith Character Success" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['a'..'z']) (parseAnyChar ['a'..'z'])) (0,0) "ohHello" `shouldBe` Right ("oh", "Hello", (0, 2))
        it "Check parseAndWith Failure" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['a'..'z']) (parseAnyChar ['0'..'9'])) (0,0) "42Hello" `shouldBe` Left  (errorParsing (0,0))
        it "Check parseMany Character Success" $ do
            runParser (parseMany (parseAnyChar ['a'..'z'])) (0,0) "bonjournoHello" `shouldBe` Right ("bonjourno", "Hello", (0,9))
        it "Check parseMany Number Success" $ do
            runParser (parseMany (parseAnyChar ['0'..'9'])) (0,0) "424554Hello" `shouldBe` Right ("424554", "Hello", (0,6))
        it "Check parseMany Failure" $ do
            runParser (parseMany (parseAnyChar ['0'..'9'])) (0,0) "Hello" `shouldBe` Right ("", "Hello", (0,0))
        it "Check parseSome Number Success" $ do
            runParser (parseSome (parseAnyChar ['0'..'9'])) (0,0) "042Hello" `shouldBe` Right ("042", "Hello", (0,3))
        it "Check parseSome Character Success" $ do
            runParser (parseSome (parseAnyChar ['a'..'z'])) (0,0) "buenos42Hello" `shouldBe` Right ("buenos", "42Hello", (0,6))
        it "Check parseSome Failure" $ do
            runParser (parseSome (parseAnyChar ['0'..'9'])) (0,0) "HelloWorld" `shouldBe` Left (errorParsing (0,0))
        it "Check parseUInt Success" $ do
            runParser parseUInt (0,0) "5463Hello" `shouldBe` Right (5463, "Hello", (0,4))
        it "Check parseUInt Failure" $ do
            runParser parseUInt (0,0) "Hola" `shouldBe` Left (errorParsing (0,0))
        it "Check parseUInt Empty" $ do
            runParser parseUInt (0,0) "" `shouldBe` Left (errorParsing (0,0))
        it "Check parseUInt Negative Value Failure" $ do
            runParser parseUInt (0,0) "-42Hello" `shouldBe` Left (errorParsing (0,0))
        it "Check parseInt Success" $ do
            runParser parseInt (0,0) "4234Hello" `shouldBe` Right (4234, "Hello", (0, 4))
        it "Check parseInt Negative Value Success" $ do
            runParser parseInt (0,0) "-42Hello" `shouldBe` Right (-42, "Hello", (0,2))
        it "Check parseInt Failure" $ do
            runParser parseInt (0,0) "Hello" `shouldBe` Left (errorParsing (0,0))
        it "Check parsesign '-' Success" $ do
            runParser parseSign (0,0) "-llg" `shouldBe` Right ('-', "llg", (0,1))
        it "Check parsesign '+' Success" $ do
            runParser parseSign (0,0) "+llg" `shouldBe` Right ('+', "llg", (0,1))
        it "Check parsesign Failure" $ do
            runParser parseSign (0,0) "lg" `shouldBe` Left (errorParsing (0,0))
        it "Check parseString Success n°1" $ do
            runParser parseString (0,0) "bonjourno " `shouldBe` Right ("bonjourno", "", (0,10))
        it "Check parseString Success n°2" $ do
            runParser parseString (0,0) "bon12*/p journo " `shouldBe` Right ("bon12", "*/p journo ", (0,5))
        it "Check parseString Failure" $ do
            runParser parseString (0,0) "^bon12*/p journo " `shouldBe` Left (errorParsing (0,0))
        it "Check parseElem with parseInt Success" $ do
            runParser (parseElem parseInt) (0,0) "12 " `shouldBe` Right (12, "", (0,3))
        it "Check parseElem with parseString Success" $ do
            runParser (parseElem parseString) (0,0) "hello la " `shouldBe` Right ("hello", "la ", (0,6))
        it "Check parseValue Success" $ do
            runParser parseValue (0,0) "432           la " `shouldBe` Right (AST.Value 432, "la ", (0,14))
        it "Check parseList String Success" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(| a,  b ,c, d   |)" `shouldBe` Right(["a", "b", "c", "d"], "", (0,19))
        it "Check parseList Error missing comma" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(| a b |)" `shouldBe` Left "Error on parsing on '0' '5'"
        it "Check parseList Error end with comma" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(|a, |)" `shouldBe` Left "Error on parsing on '0' '5'"
        it "Check parseList Error starting with comma" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(|,a|)" `shouldBe` Left "Error on parsing on '0' '2'"
        it "Check parseList Error missing starting bracket" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "a, b|)" `shouldBe` Left "Error on parsing on '0' '0'"
        it "Check parseList Error missing ending bracket" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(|a, b" `shouldBe` Left "Error on parsing on '0' '6'"
        it "Check parseList with parseString Failure" $ do
            runParser (parseList parseString "(|" "|)") (0,0) "(|buenos, 3, owow, k, ye    |)" `shouldBe` Left (errorParsing (0,10))
        it "Check parseBool true Success" $ do
            runParser parseBool (0,0) "true lp" `shouldBe` Right (AST.Boolean True, "lp", (0,5))
        it "Check parseBool false Success" $ do
            runParser parseBool (0,0) "false lp" `shouldBe` Right (AST.Boolean False, "lp", (0,6))
        it "Check parseBool Failure" $ do
            runParser parseBool (0,0) "#tlp" `shouldBe` Left (errorParsing (0,5))
        it "Check parseExpr Simple Addition Success" $ do
            runParser parseExpr (0,0) "3 + 5" `shouldBe` Right (AST.Call "+" [AST.Value 3,AST.Value 5],"",(0,5))
        it "Check parseExpr Simple Multiplication Success" $ do
            runParser parseExpr (0,0) "3 * 3" `shouldBe` Right (AST.Call "*" [AST.Value 3,AST.Value 3],"",(0,5))
        it "Check parseExpr Simple Substration Success" $ do
            runParser parseExpr (0,0) "3 - 3" `shouldBe` Right (AST.Call "-" [AST.Value 3, AST.Value 3],"",(0,5))
        it "Check parseExpr Simple Division Success" $ do
            runParser parseExpr (0,0) "3 / 3" `shouldBe` Right (AST.Call "/" [AST.Value 3, AST.Value 3],"",(0,5))
        it "Check parseExpr Simple Modulo Success" $ do
            runParser parseExpr (0,0) "3 % 3" `shouldBe` Right (AST.Call "%" [AST.Value 3, AST.Value 3],"",(0,5))
        it "Check parseExpr Multiple Addition Success" $ do
            runParser parseExpr (0, 0) "3 + 3 + 5 + 1" `shouldBe` Right (AST.Call "+" [AST.Value 3,AST.Call "+" [AST.Value 3,AST.Call "+" [AST.Value 5,AST.Value 1]]],"",(0,13))
        it "Check parseExpr Multiple Multiplication Success" $ do
            runParser parseExpr (0, 0) "3 * 3 * 5 * 4" `shouldBe` Right (AST.Call "*" [AST.Value 3,AST.Call "*" [AST.Value 3,AST.Call "*" [AST.Value 5,AST.Value 4]]],"",(0,13))
        it "Check parseExpr with parenthesis Success" $ do
            runParser parseExpr (0, 0) "3 * (| 3 + 4 |)" `shouldBe` Right (AST.Call "*" [AST.Value 3,AST.Call "+" [AST.Value 3,AST.Value 4]],"",(0,15))
        it "Check parseExpr with Multiple parenthesis Success n°1" $ do
            runParser parseExpr (0, 0) "3 * (| 3 + (| 3 * (| 6 - 2 |)|)|)" `shouldBe` Right (AST.Call "*" [AST.Value 3,AST.Call "+" [AST.Value 3,AST.Call "*" [AST.Value 3,AST.Call "-" [AST.Value 6,AST.Value 2]]]],"",(0,33))
        it "Check parseExpr with Multiple parenthesis Success n°2" $ do
            runParser parseExpr (0, 0) "3 + (| 64 - 34 |) + 54 * 43" `shouldBe` Right (AST.Call "+" [AST.Value 3,AST.Call "+" [AST.Call "-" [AST.Value 64,AST.Value 34],AST.Call "*" [AST.Value 54,AST.Value 43]]],"",(0,27))
        it "Check parseExpr with Multiple parenthesis Success n°3" $ do
            runParser parseExpr (0, 0) "(| 34 + (| 43 - 123 |)|) + (| 4 + (| 23 - 4 |)|)" `shouldBe` Right (AST.Call "+" [AST.Call "+" [AST.Value 34,AST.Call "-" [AST.Value 43,AST.Value 123]],AST.Call "+" [AST.Value 4,AST.Call "-" [AST.Value 23,AST.Value 4]]],"",(0,48))
        it "Check parseExpr with Operator '==' Success" $ do
            runParser parseExpr (0, 0) "3 == 3" `shouldBe` Right (AST.Call "==" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '>=' Success" $ do
            runParser parseExpr (0, 0) "3 >= 3" `shouldBe` Right (AST.Call ">=" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '<=' Success" $ do
            runParser parseExpr (0, 0) "3 <= 3" `shouldBe` Right (AST.Call "<=" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '>' Success" $ do
            runParser parseExpr (0, 0) "3 > 3" `shouldBe` Right (AST.Call ">" [AST.Value 3,AST.Value 3],"",(0,5))
        it "Check parseExpr with Operator '<' Success" $ do
            runParser parseExpr (0, 0) "3 < 3" `shouldBe` Right (AST.Call "<" [AST.Value 3,AST.Value 3],"",(0,5))
        it "Check parseExpr with Operator '!=' Success" $ do
            runParser parseExpr (0, 0) "3 != 3" `shouldBe` Right (AST.Call "!=" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '!!' Success" $ do
            runParser parseExpr (0, 0) "3 !! 3" `shouldBe` Right (AST.Call "!!" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '++' Success" $ do
            runParser parseExpr (0, 0) "3 ++ 3" `shouldBe` Right (AST.Call "++" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '--' Success" $ do
            runParser parseExpr (0, 0) "3 -- 3" `shouldBe` Right (AST.Call "--" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '&&' Success" $ do
            runParser parseExpr (0, 0) "3 && 3" `shouldBe` Right (AST.Call "&&" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '||' Success" $ do
            runParser parseExpr (0, 0) "3 || 3" `shouldBe` Right (AST.Call "||" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '^^' Success" $ do
            runParser parseExpr (0, 0) "3 ^^ 3" `shouldBe` Right (AST.Call "^^" [AST.Value 3,AST.Value 3],"",(0,6))
        it "Check parseExpr with Operator '$' Success" $ do
            runParser parseExpr (0, 0) "3 $ 3" `shouldBe` Right (AST.Call "$" [AST.Value 3,AST.Value 3],"",(0,5))
        it "Check parseExpr Big Expression Success n°1" $ do
            runParser parseExpr (0, 0) "(|3 + (| 4 * 23 |) |) == (|3 + 5 + (|54 - 2|)|) && (| 4 * 43 |)" `shouldBe` Right (AST.Call "&&" [AST.Call "==" [AST.Call "+" [AST.Value 3,AST.Call "*" [AST.Value 4,AST.Value 23]],AST.Call "+" [AST.Value 3,AST.Call "+" [AST.Value 5,AST.Call "-" [AST.Value 54,AST.Value 2]]]],AST.Call "*" [AST.Value 4,AST.Value 43]],"",(0,63))
        it "Check parseExpr Big Expression Success n°2" $ do
            runParser parseExpr (0, 0) "(|3 + (| 4 * 23 |) |) == (|3 + 5 + (|54 - 2|)|) && (| 4 * 43 |) $ (|3 + 3 * (| 65 - 4 |) |)" `shouldBe` Right (AST.Call "$" [AST.Call "&&" [AST.Call "==" [AST.Call "+" [AST.Value 3,AST.Call "*" [AST.Value 4,AST.Value 23]],AST.Call "+" [AST.Value 3,AST.Call "+" [AST.Value 5,AST.Call "-" [AST.Value 54,AST.Value 2]]]],AST.Call "*" [AST.Value 4,AST.Value 43]],AST.Call "+" [AST.Value 3,AST.Call "*" [AST.Value 3,AST.Call "-" [AST.Value 65,AST.Value 4]]]],"",(0,91))
        it "Check parseExpr Define Value Success" $ do
            runParser parseDefineValue (0,0) "a = 3" `shouldBe` Right (AST.Define "a" (AST.Value 3),"",(0,5))
        it "Check parseExpr Define Expression Success" $ do
            runParser parseDefineValue (0,0) "a = (| 3 + 5 |)" `shouldBe` Right (AST.Define "a" (AST.Call "+" [AST.Value 3,AST.Value 5]),"",(0,15))
        it "Check parseExpr Define Value Boolean Success" $ do
            runParser parseDefineValue (0, 0) "a=true" `shouldBe` Right (AST.Define "a" (AST.Boolean True),"",(0,6))
        it "Check parseExpr Define Failure (incorrect value name)" $ do
            runParser parseDefineValue (0, 0) "23=true" `shouldBe` Left (errorParsing (0,0))
        it "Check parseExpr Define Failure (missing operator '=')" $ do
            runParser parseDefineValue (0, 0) "a " `shouldBe` Left (errorParsing (0,2))
        it "Check parseExpr Define Failure (missing AST Value)" $ do
            runParser parseDefineValue (0, 0) "a =" `shouldBe` Left (errorParsing (0,3))
        it "Check parseExpr String Value Success" $ do
            runParser parseAstString (0, 0) "\"a\"" `shouldBe` Right (AST.String "a" ,"",(0,3))
        it "Check parseExpr WhiteSpace Value Success" $ do
            runParser parseWhiteSpace (0, 0) "\t\t\n" `shouldBe` Right ("\t\t\n" ,"",(1,0))
        it "Check parseBool Failure" $ do
            runParser parseBool (0, 0) "fla" `shouldBe` Left (errorParsing (0,0))
        it "Check parseTrue Failure" $ do
            runParser parseTrue (0, 0) "fla" `shouldBe` Left (errorParsing (0,0))
        it "Check parseLobster Success" $ do
            runParser parseLobster (0,0) "a = 3 \"a\"" `shouldBe` Right ([AST.Define "a" (AST.Value 3),AST.String "a"],"",(0,9))
        it "Check parseString Success" $ do
            runParser parseString (0,0) "_Lob3ster*" `shouldBe` Right ("_Lob3ster","*",(0,9))
        it "Check parseMany Success" $ do
            runParser (parseMany (parseChar ' ')) (0,0) " p" `shouldBe` Right (" ","p",(0,1))
        it "Check parseExpr Unary Operation Success" $ do
            runParser parseExpr (0,0) "! true" `shouldBe` Right (AST.Call "!" [AST.Boolean True],"",(0,6))
        it "Check parseExpr Unary Operation Failure (incorrect AST)" $ do
            runParser parseExpr (0,0) "! *" `shouldBe` Left (errorParsing (0,0))
        it "Check parseExpr Unary Operation Failure (missing operator)" $ do
            runParser parseExpr (0,0) "error" `shouldBe` Right (AST.Symbol "error" Nothing, "", (0, 5))
        it "Check parseCmpString Success" $ do
            runParser (parseCmpString "test") (0,0) "test" `shouldBe` Right ("test", "", (0, 4))
        it "Check parseCmpString Success with remaining string" $ do
            runParser (parseCmpString "test") (0,0) "test abc" `shouldBe` Right ("test", "abc", (0, 5))
        it "Check parseCmpString Failure" $ do
            runParser (parseCmpString "test") (0,0) "testa abc" `shouldBe` Left (errorParsing (0, 0))
        it "Check parseFunctionValue Success" $ do
            runParser parseFunctionValue (0,0) "(|a,b|) {| a + b |}" `shouldBe` Right (AST.FunctionValue ["a","b"] (AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) Nothing,"",(0,19))
        it "Check parseFunctionValue Failure missing brackets" $ do
            runParser parseFunctionValue (0,0) "(|a,b|) a + b" `shouldBe` Left (errorParsing (0, 8))
        it "Check parseFunctionValue Failure parameters" $ do
            runParser parseFunctionValue (0,0) "{| a + b |}" `shouldBe` Left (errorParsing (0, 0))
        it "Check parseFunctionValue Failure empty brackets" $ do
            runParser parseFunctionValue (0,0) "(|a,b|) {||}" `shouldBe` Left (errorParsing (0, 10))
        it "Check parseFunctionValue Success no parameter" $ do
            runParser parseFunctionValue (0,0) "(||) {|1|}" `shouldBe` Right (AST.FunctionValue [] (AST.Value 1) Nothing, "", (0,10))
        it "Check parseBracket Success" $ do
            runParser parseBracket (0,0) "{| a + b |}" `shouldBe` Right ((AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]), "", (0, 11))
        it "Check parseBracket Failure invalid start bracket" $ do
            runParser parseBracket (0,0) "{ a + b |}" `shouldBe` Left (errorParsing (0, 1))
        it "Check parseBracket Failure invalid end bracket" $ do
            runParser parseBracket (0,0) "{| a + b }" `shouldBe` Left (errorParsing (0, 9))
        it "Check parseBracket Failure empty brackets" $ do
            runParser parseBracket (0,0) "{||}" `shouldBe` Left (errorParsing (0, 2))
        it "Check parseLambda Success" $ do
            runParser parseLambda (0,0) "lambda(|a,b|) {| a + b |}" `shouldBe` Right (AST.FunctionValue ["a","b"] (AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) Nothing,"",(0,25))
        it "Check parseLambda Success λ" $ do
            runParser parseLambda (0,0) "λ(|a,b|) {| a + b |}" `shouldBe` Right (AST.FunctionValue ["a","b"] (AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) Nothing,"",(0,20))
        it "Check parseLambda Failure missing lambda/λ" $ do
            runParser parseLambda (0,0) "(|a,b|) {| a + b |}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseLambda Failure invalid keyword" $ do
            runParser parseLambda (0,0) "lambdaa(||) {|1|}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseLambda Failure word after λ" $ do
            runParser parseLambda (0,0) "λ a(||) {|1|}" `shouldBe` Left (errorParsing (0,2))
        it "Check parseLambda Failure empty brackets" $ do
            runParser parseLambda (0,0) "λ(||) {||}" `shouldBe` Left (errorParsing (0,8))
        it "Check parseCond Success single if" $ do
            runParser parseCond (0,0) "if a == b {| 1 |}" `shouldBe` Right (AST.Cond (AST.Call "==" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) (AST.Value 1) Nothing,"",(0,17))
        it "Check parseCond Success if and else" $ do
            runParser parseCond (0,0) "if a == b {| 1 |} else {| 0 |}" `shouldBe` Right ((AST.Cond (AST.Call "==" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) (AST.Value 1) (Just (AST.Value 0))),"",(0,30))
        it "Check parseCond Success if, else if and else" $ do
            runParser parseCond (0,0) "if a == b {| 1 |} else if a == 2 {| 2 |} else {| 0 |}" `shouldBe` Right (AST.Cond (AST.Call "==" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) (AST.Value 1) (Just (AST.Cond (AST.Call "==" [AST.Symbol "a" Nothing, AST.Value 2]) (AST.Value 2) (Just (Value 0)))),"",(0,53))
        it "Check parseCond Failure invalid expr" $ do
            runParser parseCond (0,0) "if a *= 2 {|1|}" `shouldBe` Left (errorParsing (0,5))
        it "Check parseCond Failure no expr" $ do
            runParser parseCond (0,0) "if {|1|}" `shouldBe` Left (errorParsing (0,3))
        it "Check parseCond Failure empty brackets" $ do
            runParser parseCond (0,0) "if a {||}" `shouldBe` Left (errorParsing (0,7))
        it "Check parseCond Failure no if" $ do
            runParser parseCond (0,0) "a == b {||}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseCond Failure no space between if and expr" $ do
            runParser parseCond (0,0) "ifa==b {||}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseCond Success ignore error in else" $ do
            runParser parseCond (0,0) "if a==b {|1|} else" `shouldBe` Right (AST.Cond (AST.Call "==" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) (AST.Value 1) Nothing, "else", (0, 14))
        it "Check parseComment Success" $ do
            runParser parseComment (0,0) "#fn test(||) {|1|}" `shouldBe` Right ('\n', "", (0, 19))
        it "Check parseComment Failure no #" $ do
            runParser parseComment (0,0) "fn test(||) {|1|}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseComment Success with new line" $ do
            runParser parseComment (0,0) "#fn test(||) {|1|}\ntest=1" `shouldBe` Right ('\n', "test=1", (1, 0))
        it "Check parseDefineFn Success" $ do
            runParser parseDefineFn (0,0) "fn function(|a|) {|a|}" `shouldBe` Right (AST.Define "function" (AST.FunctionValue ["a"] (AST.Symbol "a" Nothing) Nothing),"",(0,22))
        it "Check parseDefineFn Success" $ do
            runParser parseDefineFn (0,0) "fn function(|a|) {|a|}" `shouldBe` Right (AST.Define "function" (AST.FunctionValue ["a"] (AST.Symbol "a" Nothing) Nothing),"",(0,22))
        it "Check parseDefineFn Failure no fn" $ do
            runParser parseDefineFn (0,0) "function(|a|) {|a|}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseDefineFn Failure no space after fn" $ do
            runParser parseDefineFn (0,0) "fnfunction(|a|) {|a|}" `shouldBe` Left (errorParsing (0,0))
        it "Check parseDefineFn Failure no function name" $ do
            runParser parseDefineFn (0,0) "fn (|a|) {|a|}" `shouldBe` Left (errorParsing (0,3))
        it "Check parseDefineFn Failure no parameter" $ do
            runParser parseDefineFn (0,0) "fn function {|a|}" `shouldBe` Left (errorParsing (0,12))
        it "Check parseDefineFn Failure no brackets" $ do
            runParser parseDefineFn (0,0) "fn function(|a|)" `shouldBe` Left (errorParsing (0,16))
        it "Check parseDefineFn Failure invalid parameters" $ do
            runParser parseDefineFn (0,0) "fn function(|a,|) {|a|}" `shouldBe` Left (errorParsing (0,15))
        it "Check parseDefineFn Failure empty brackets" $ do
            runParser parseDefineFn (0,0) "fn function(|a|) {||}" `shouldBe` Left (errorParsing (0,19))
