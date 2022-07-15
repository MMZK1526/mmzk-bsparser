import           MMZK.BSParser
import qualified MMZK.BSParser.ASCII as PA
import qualified MMZK.BSParser.Lexer as L
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList [test1, test2]
  where
    test1 = TestLabel "Wordlist 1"
          $ TestList [ testSingleValid1 sithList sith
                     , testSingleInvalid1 sithE ]
    test2 = TestLabel "Wordlist 2"
          $ TestList [ testSingleValid2 sithList sith
                     , testSingleValid2 sithList sithE ]

testSingleValid1 :: [String] -> String -> Test
testSingleValid1 exp str = TestCase 
                         $ assertEqual ("Test parsing " ++ show str ++ ":")
                                       (Just exp) (parseWordlist1 str)

testSingleInvalid1 :: String -> Test
testSingleInvalid1 str = TestCase
                       $ assertEqual ("Test parsing " ++ show str ++ ":")
                                     Nothing (parseWordlist1 str)

testSingleValid2 :: [String] -> String -> Test
testSingleValid2 exp str = TestCase 
                         $ assertEqual ("Test parsing" ++ show str ++ ":")
                                       (Just exp) (parseWordlist2 str)

testSingleInvalid2 :: String -> Test
testSingleInvalid2 str = TestCase
                       $ assertEqual ("Test parsing " ++ show str ++ ":")
                                     Nothing (parseWordlist2 str)

sith, sithE :: String
sith  = "Darth,Sidious,Maul,Tyranus,Vader"
sithE = " Darth, Sidious, Maul, Tyranus, Vader "

sithList :: [String]
sithList = ["Darth", "Sidious", "Maul", "Tyranus", "Vader"]

-- | Parse a Wordlist, which is a list of words (formed by English letters)
-- separated by ','s without any extra space. For example, "foo", "apple,bear",
-- and "Darth,Sidious,Maul,Tyranus,Vader" are Wordlists, while "   spaces",
-- "I-have-dashes", "spaces, after, comma" and "trailing,comma," are not.
parseWordlist1 :: ByteStringLike s => s -> Maybe [String]
parseWordlist1 = parse (wordlistParser1 <* eof)
  where
    wordlistParser1 = sepBy (PA.char ',') (some PA.alpha)

-- | Parse a Wordlist, but allows extra spaces (' ') anywhere between the words.
parseWordlist2 :: ByteStringLike s => s -> Maybe [String]
parseWordlist2 = parse (wordlistParser2 <* eof)
  where
    wordlistParser2 = do
      setSpaceParser (many PA.space32)
      lexer $ pure () -- Parse leading spaces
      sepBy (lexer $ PA.char ',') (lexer $ some PA.alpha)
