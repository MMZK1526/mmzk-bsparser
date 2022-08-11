import           Data.Char
import           MMZK.BSParser
import qualified MMZK.BSParser.Lexer as L

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

test :: Show a => Parser a -> String -> IO ()
test parser inputStr = putStrLn $ case parse (parser <* L.eof) inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err

data Kuohu = Empty
           | Apply [Kuohu]
           | Paren Kuohu
           | Brack Kuohu
  deriving Show

-- The "Kuohu" parser.
kuohuParser :: Parser Kuohu
kuohuParser = do
  kuohus <- many singleParser -- "singleParser" parses a single nested component
  return $ case kuohus of
    []      -> Empty
    [kuohu] -> kuohu
    _       -> Apply kuohus

-- | The "Single" parser.
singleParser :: Parser Kuohu
singleParser = choice [ Paren <$> parens (L.char '(') (L.char ')') kuohuParser
                      , Brack <$> parens (L.char '[') (L.char ']') kuohuParser ]
