module ExercisesParsingPractice where

import           Text.Parser.Combinators (eof)
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- 1
main' = do
  pNL "one:"
  testEOF $ one >> eof
  pNL "oneTwo:"
  testEOF $ oneTwo >> eof

-- 2
oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

testParseS :: Parser String -> String -> IO ()
testParseS p s = print $ parseString p mempty s

main'' = do
  pNL "oneS 1:"
  testParseS oneS "1"
  pNL "oneS 12:"
  testParseS oneS "12"
  pNL "oneS 123:"
  testParseS oneS "123"
  pNL "oneS choice:"
  testParseS (choice [oneS, oneTwoS, oneTwoThreeS]) "123"
