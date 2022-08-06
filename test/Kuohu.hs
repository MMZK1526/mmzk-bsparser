import           Base
import           MMZK.BSParser
import qualified MMZK.BSParser.ASCII as PA
import qualified MMZK.BSParser.CPS as CPS
import           MMZK.BSParser.Error
import           Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit

-- | Example: Nested parenthesis, brackets, braces, and chevrons.
data Kuohu = Empty
           | Apply [Kuohu]
           | Paren Kuohu
           | Brack Kuohu
           | Brace Kuohu
           | Chevr Kuohu

instance Semigroup Kuohu where
  Empty <> kuohu       = kuohu
  kuohu <> Empty       = kuohu
  Apply k1 <> Apply k2 = Apply $ k1 ++ k2
  kuohu1 <> kuohu2     = Apply [kuohu1, kuohu2]

instance Monoid Kuohu where
  mempty = Empty

instance Show Kuohu where
  show Empty          = ""
  show (Apply kuohus) = concatMap show kuohus
  show (Paren kuohu)  = "(" ++ show kuohu ++ ")"
  show (Brack kuohu)  = "[" ++ show kuohu ++ "]"
  show (Brace kuohu)  = "{" ++ show kuohu ++ "}"
  show (Chevr kuohu)  = "<" ++ show kuohu ++ ">"

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
     $ TestList [ testBasicPairs , testAdvancedPairs, testMalformedPairs ]
  where
    basicPairs     = [simpleParens, simpleBraces, simpleBracks, simpleChevrs]
    advancedPairs  = [consecutivePairs, nestedPairs, combo]
    malformedPairs = [mismatchPairs, badOrder]

testBasicPairs :: Test
testBasicPairs = TestLabel "Basic pairs"
               $ TestList [ testValid' parseKuohu simpleParens
                          , testValid' parseKuohu simpleBraces
                          , testValid' parseKuohu simpleBracks
                          , testValid' parseKuohu simpleChevrs ]

testAdvancedPairs :: Test
testAdvancedPairs = TestLabel "Advanced pairs"
                  $ TestList [ testValid' parseKuohu consecutivePairs
                             , testValid' parseKuohu nestedPairs
                             , testValid' parseKuohu combo ]

testMalformedPairs :: Test
testMalformedPairs = TestLabel "Malformed pairs"
                   $ TestList [ testInvalid parseKuohu [err1] mismatchPairs
                              , testInvalid parseKuohu [err2] badOrder ]
  where
    err1 = ErrSpan
        { esLoc   = (14, 14)
        , esError = BasicErr
            { unexpected  = UItem Nothing (Just (CToken '}'))
            , expecting   = M.fromList [( Nothing
                                        , S.fromList [ CToken '(', CToken ')'
                                                     , CToken '<', CToken '['
                                                     , CToken '{' ] )]
            , errMessages = [] } }
    err2 = ErrSpan
        { esLoc   = (0, 0)
        , esError = BasicErr
            { unexpected  = UItem Nothing (Just (CToken ')'))
            , expecting   = M.fromList [( Nothing
                                        , S.fromList [ CToken '(', CToken '<'
                                                     , CToken '[', CToken '{'
                                                     , EOI ] )]
            , errMessages = [] } }

parseKuohu :: ByteStringLike s => s -> Either (ErrBundle String) Kuohu
parseKuohu = parse (CPS.wrapper (pure ()) kuohuCPS)

kuohuCPS :: Parser Kuohu -> Parser Kuohu
kuohuCPS = CPS.manyS ((prune >>) . singleNestParser)
  where
    singleNestParser = CPS.choice
        [ fmap Paren . CPS.parens (PA.char '(') (PA.char ')') kuohuCPS
        , fmap Brack . CPS.parens (PA.char '[') (PA.char ']') kuohuCPS
        , fmap Brace . CPS.parens (PA.char '{') (PA.char '}') kuohuCPS
        , fmap Chevr . CPS.parens (PA.char '<') (PA.char '>') kuohuCPS ]
