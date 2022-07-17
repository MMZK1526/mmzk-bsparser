import           MMZK.BSParser
import qualified MMZK.BSParser.ASCII as PA
import           Data.Either
import           Test.HUnit

-- | Example: Nested parenthesis, brackets, braces, and chevrons.
data Kuohu = Empty
           | Apply [Kuohu]
           | Paren Kuohu
           | Brack Kuohu
           | Brace Kuohu
           | Chevr Kuohu
  deriving Show

-- | Basic pair examples.
simpleParens, simpleBracks, simpleBraces, simpleChevrs :: String
simpleParens = "()"
simpleBracks = "[]"
simpleBraces = "{}"
simpleChevrs = "<>"

-- | Valid examples.
consecutivePairs, nestedPairs, combo :: String
consecutivePairs = "()[]<>{}[]"
nestedPairs      = "([{<>}])"
combo            = "({}{}<{}>[{<{}><[]>[()]()}])({}{}<{}>[{<{}><[]>[()]()}])"

-- | Malformed examples.
mismatchPairs, badOrder :: String
mismatchPairs = "<<>[](([{}<>])}>"
badOrder      = ")("

main :: IO ()
main = runTestTTAndExit
     $ TestList [ testValids "Basic pairs" basicPairs
                , testValids "Advanced pairs" advancedPairs
                , testInvalids "Malformed pairs" malformedPairs ]
  where
    basicPairs     = [simpleParens, simpleBraces, simpleBracks, simpleChevrs]
    advancedPairs  = [consecutivePairs, nestedPairs, combo]
    malformedPairs = [mismatchPairs, badOrder]

testValids :: String -> [String] -> Test
testValids = (. TestList . map testSingleValid) . TestLabel

testInvalids :: String -> [String] -> Test
testInvalids = (. TestList . map testSingleInvalid) . TestLabel

testSingleValid :: String -> Test
testSingleValid kuohu = TestCase
                      $ assertEqual ("Test parsing \"" ++ kuohu ++ "\":")
                                    (Right kuohu)
                                    (fmap formatKuohu (parseKuohu kuohu))

testSingleInvalid :: String -> Test
testSingleInvalid kuohu = TestCase
                        $ assertBool ("Test parsing \"" ++ kuohu ++ "\":")
                                      (isLeft $ parseKuohu kuohu)

formatKuohu :: Kuohu -> String
formatKuohu Empty          = ""
formatKuohu (Apply kuohus) = concatMap formatKuohu kuohus
formatKuohu (Paren kuohu)  = "(" ++ formatKuohu kuohu ++ ")"
formatKuohu (Brack kuohu)  = "[" ++ formatKuohu kuohu ++ "]"
formatKuohu (Brace kuohu)  = "{" ++ formatKuohu kuohu ++ "}"
formatKuohu (Chevr kuohu)  = "<" ++ formatKuohu kuohu ++ ">"

parseKuohu :: ByteStringLike s => s -> Either [ErrorSpan] Kuohu
parseKuohu = parse (kuohuParser <* eof)

kuohuParser :: Parser Kuohu
kuohuParser = do
  -- Print a message whenever encoutering an application
  kuohus <- many singleNestParser
  return $ case length kuohus of
    0 -> Empty
    1 -> head kuohus
    _ -> Apply kuohus
  where
    singleNestParser
      = choice [ Paren <$> parens (PA.char '(') (PA.char ')') kuohuParser
               , Brack <$> parens (PA.char '[') (PA.char ']') kuohuParser
               , Brace <$> parens (PA.char '{') (PA.char '}') kuohuParser
               , Chevr <$> parens (PA.char '<') (PA.char '>') kuohuParser ]
