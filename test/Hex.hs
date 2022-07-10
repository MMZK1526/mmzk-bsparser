import           Control.Monad
import           MMZKBSParser
import qualified MMZKBSParser.Lexer as L
import           Test.HUnit

main = print (parseHex "BD")

-- main :: IO ()
-- main = runTestTTAndExit
--      $ TestList [ testValids "Basic pairs" basicPairs
--                 , testValids "Advanced pairs" advancedPairs
--                 , testInvalids "Malformed pairs" malformedPairs ]
--   where
--     basicPairs     = [simpleParens, simpleBraces, simpleBracks, simpleChevrs]
--     advancedPairs  = [consecutivePairs, nestedPairs, combo]
--     malformedPairs = [mismatchPairs, badOrder]

-- testValids :: String -> [String] -> Test
-- testValids = (. TestList . map testSingleValid) . TestLabel

-- testInvalids :: String -> [String] -> Test
-- testInvalids = (. TestList . map testSingleInvalid) . TestLabel

-- testSingleValid :: String -> Test
-- testSingleValid kuohu = TestCase
--                       $ assertEqual ("Test parsing \"" ++ kuohu ++ "\":")
--                                     (Just kuohu)
--                                     (fmap formatKuohu (parseKuohu kuohu))

-- testSingleInvalid :: String -> Test
-- testSingleInvalid kuohu = TestCase
--                         $ assertEqual ("Test parsing \"" ++ kuohu ++ "\":")
--                                       Nothing
--                                       (fmap formatKuohu (parseKuohu kuohu))

parseHex :: ByteStringLike s => s -> Maybe String
parseHex = parse (hexParser <* eof)

hexParser :: Parser String
hexParser = many (L.hexDigit <&> [L.upper])

parseAlphaHex :: ByteStringLike s => s -> Maybe String
parseAlphaHex = parse (alphaHexParser <* eof)

alphaHexParser :: Parser String
alphaHexParser = many (L.hexDigit <&> [void L.upper, neg L.digit])
