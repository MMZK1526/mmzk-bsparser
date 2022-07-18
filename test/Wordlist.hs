import           MMZK.BSParser
import qualified MMZK.BSParser.ASCII as PA
import qualified MMZK.BSParser.Lexer as L
import           Data.Either
import           Test.HUnit

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

main :: IO ()
main = runTestTTAndExit $ TestList [test1, test2, test3]
  where
    test1 = TestLabel "Wordlist 1"
          $ TestList [ testSingleValid1 sithList sith
                     , testSingleInvalid1 sithE
                     , testSingleInvalid1 jedi ]
    test2 = TestLabel "Wordlist 2"
          $ TestList [ testSingleValid2 sithList sith
                     , testSingleValid2 sithList sithE
                     , testSingleInvalid1 jedi ]
    test3 = TestLabel "Wordlist 3"
          $ TestList [ testSingleValid3 sithList sith
                     , testSingleValid3 sithList sithE
                     , testSingleValid3 jediList jedi ]

testSingleValid1 :: [String] -> String -> Test
testSingleValid1 es str = TestCase 
                         $ assertEqual ("Test parsing " ++ show str ++ ":")
                                       (Right es) (parseWordlist1 str)

testSingleInvalid1 :: String -> Test
testSingleInvalid1 str = TestCase
                       $ assertBool ("Test parsing " ++ show str ++ ":")
                                    (isLeft $ parseWordlist1 str)

testSingleValid2 :: [String] -> String -> Test
testSingleValid2 es str = TestCase 
                         $ assertEqual ("Test parsing" ++ show str ++ ":")
                                       (Right es) (parseWordlist2 str)

testSingleInvalid2 :: String -> Test
testSingleInvalid2 str = TestCase
                       $ assertBool ("Test parsing " ++ show str ++ ":")
                                    (isLeft $ parseWordlist2 str)

testSingleValid3 :: [String] -> String -> Test
testSingleValid3 es str = TestCase 
                         $ assertEqual ("Test parsing" ++ show str ++ ":")
                                       (Right es) (parseWordlist3 str)

testSingleInvalid3 :: String -> Test
testSingleInvalid3 str = TestCase
                       $ assertBool ("Test parsing " ++ show str ++ ":")
                                    (isLeft $ parseWordlist3 str)

sith, sithE, jedi :: String
sith  = "Darth,Sidious,Maul,Tyranus,Vader"
sithE = " Darth, Sidious, Maul, Tyranus, Vader "
jedi  = "Obi-Wan, Infil'a, Yoda"

sithList, jediList :: [String]
sithList = ["Darth", "Sidious", "Maul", "Tyranus", "Vader"]
jediList = ["Obi-Wan", "Infil'a", "Yoda"]

-- | Parse a Wordlist, which is a list of words (formed by English letters)
-- separated by ','s without any extra space. For example, "foo", "apple,bear",
-- and "Darth,Sidious,Maul,Tyranus,Vader" are Wordlists, while "   spaces",
-- "I-have-dashes", "spaces, after, comma" and "trailing,comma," are not.
parseWordlist1 :: ByteStringLike s => s -> Either [ErrSpan String] [String]
parseWordlist1 = parse (wordlistParser1 <* L.eof)

wordlistParser1 :: Parser [String]
wordlistParser1 = sepBy (PA.char ',') (some PA.alpha)

-- | Parse a Wordlist, but allows extra spaces (' ') anywhere between the words.
parseWordlist2 :: ByteStringLike s => s -> Either [ErrSpan String] [String]
parseWordlist2 = parse (wordlistParser2 <* L.eof)

wordlistParser2 :: Parser [String]
wordlistParser2 = do
  setSpaceParser (many PA.space32)
  lexer $ pure () -- Parse leading spaces
  sepBy (lexer $ PA.char ',') (lexer $ some PA.alpha)

-- | Parse a Wordlist, but allows the words to contain "'" and "-".
parseWordlist3 :: ByteStringLike s => s -> Either [ErrSpan String] [String]
parseWordlist3 = parse (wordlistParser3 <* L.eof)

wordlistParser3 :: Parser [String]
wordlistParser3 = do
  setSpaceParser (many PA.space32)
  lexer $ pure () -- Parse leading spaces
  sepBy (lexer $ PA.char ',') 
        (lexer $ some (choice [PA.alpha, PA.char '-', PA.char '\'']))
