module Base where

import           MMZK.BSParser
import           MMZK.BSParser.Error
import           Test.HUnit

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

testValid :: ByteStringLike s => Show s => Show a => Eq a
          => (s -> Either (ErrBundle String) a)
          -> a -> s -> Test
testValid p expected str = TestCase $ do
  case p str of
    Left _  -> assertFailure $ show str ++ " should succeed"
    Right a -> assertEqual ("Test parsing " ++ show str ++ ":") expected a

testInvalid :: ByteStringLike s => Show s
            => (s -> Either (ErrBundle String) a)
            -> [ErrSpan String] -> s -> Test
testInvalid p errs str = TestCase $ do
  case p str of
    Right _ -> assertFailure $ show str ++ " should fail"
    Left eb -> let errs' = ebErrors eb
               in  assertEqual ("Test parsing " ++ show str ++ ":")
                               (esLoc <$> errs, esError <$> errs)
                               (esLoc <$> errs', esError <$> errs')
