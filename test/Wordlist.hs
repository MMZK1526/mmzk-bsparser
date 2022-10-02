import           Base
import           MMZK.BSParser
import           MMZK.BSParser.Error
import qualified MMZK.BSParser.CPS as CPS
import qualified MMZK.BSParser.ASCII as PA
import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList [test1, test2, test3]
  where
    test1 = TestLabel "Wordlist 1"
          $ TestList [ testValid parseWordlist1 sithList sith
                     , testInvalid parseWordlist1 [err2] sithE
                     , testInvalid parseWordlist1 [err1] jedi ]
    test2 = TestLabel "Wordlist 2"
          $ TestList [ testValid parseWordlist2 sithList sith
                     , testValid parseWordlist2 sithList sithE
                     , testInvalid parseWordlist2 [err1] jedi ]
    test3 = TestLabel "Wordlist 3"
          $ TestList [ testValid parseWordlist3 sithList sith
                     , testValid parseWordlist3 sithList sithE
                     , testValid parseWordlist3 jediList jedi ]
    err1  = ErrSpan 
        { esLoc   = (3, 3)
        , esError = BasicErr 
            { unexpected  = UItem Nothing (Just (CToken '-'))
            , expecting   = M.fromList [(Nothing, S.fromList [EOI])]
            , errMessages = [] } }
    err2  = ErrSpan 
        { esLoc   = (0, 0)
        , esError = BasicErr 
            { unexpected  = UItem Nothing (Just (CToken ' '))
            , expecting   = M.fromList [(Nothing, S.fromList [EOI])]
            , errMessages = [] } }

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
parseWordlist1 :: ByteStringLike s => s -> Either (ErrBundle String) [String]
parseWordlist1 = parse (wordlistParser1 <* PA.eof)

wordlistParser1 :: Parser [String]
wordlistParser1 = sepBy (PA.char ',') (some PA.alpha)

-- | Parse a Wordlist, but allows extra spaces (' ') anywhere between the words.
parseWordlist2 :: ByteStringLike s => s -> Either (ErrBundle String) [String]
parseWordlist2 = parse (wordlistParser2 <* PA.eof)

wordlistParser2 :: Parser [String]
wordlistParser2 = do
  setSpaceParser (many PA.space32)
  lexer $ pure () -- Parse leading spaces
  sepBy (lexer $ PA.char ',') (lexer $ some PA.alpha)

-- | Parse a Wordlist, but allows the words to contain non-consecutive "'" and
-- "-".
parseWordlist3 :: ByteStringLike s => s -> Either (ErrBundle String) [String]
parseWordlist3 = parse (wordlistParser3 <* PA.eof)

wordlistParser3 :: Parser [String]
wordlistParser3 = do
  setSpaceParser (many PA.space32)
  lexer $ pure () -- Parse leading spaces
  sepBy (lexer $ PA.char ',') (prune >> lexer wordParser)
  where
    wordParser = ( CPS.some PA.alpha
                 . CPS.manyS ( (pruneNext >>)
                             . CPS.cons (choice [PA.char '-', PA.char '\''])
                             . CPS.some PA.alpha ) ) (pure [])
