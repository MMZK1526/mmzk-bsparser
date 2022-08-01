import           Base
import           MMZK.BSParser
import           MMZK.BSParser.Error
import qualified MMZK.BSParser.CPS as CPS
import qualified MMZK.BSParser.Lexer as L
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit

main :: IO ()
main = print 42

data JSON = Obj (Map String JSON)
          | Arr [JSON]
          | Str String
          | Num Double
          | JTrue | JFalse | JNull

-- | Parse a JSON number, which has the form
-- "-?(0|(1-9)(0-9)*)(\.(0-9)+)([eE](+/-)?(0-9)+)".
jNum :: Fractional a => Monad m => BSParserT e m a
jNum = do
  minusSign <- optional $ L.char '-'
  case minusSign of
    Nothing -> jUNum
    Just _  -> negate <$> jUNum
  where
    jUNum = do
      m0 <- optional $ L.char '0'
      coePart <- case m0 of
        Just _  -> choice [L.decimate (pure 0) L.fractional, 0 <$ L.neg L.digit]
        Nothing -> do
          intPart <- L.digits
          choice [L.decimate (pure intPart) L.fractional, pure intPart]
      choice [ L.scientify 10 (pure coePart) (choice [ L.signed L.digits
                                                     , pure (0 :: Int) ])
             , pure coePart ]

-- | Parse "true", "false", and "null".
jLit :: Monad m => BSParserT e m JSON
jLit = pmap (`lookup` [("true", JTrue), ("false", JFalse), ("null", JNull)])
            L.alphas
