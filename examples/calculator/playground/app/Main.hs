import           Data.Char
import           MMZK.BSParser
import qualified MMZK.BSParser.Lexer as L
import System.Environment
import System.Exit

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> die "请输入算式！"
    a : _ -> test calculator a

test :: Show a => Parser a -> String -> IO ()
test parser inputStr = case parse (L.wrapper (many L.space) parser) inputStr of
  Right a  -> print a
  Left err -> die (renderErrBundleAsStr err)

letterParser :: Parser Char
letterParser = L.satisfy (\ch -> isAlpha ch && isAscii ch) <?> ["ascii letter"]

calculator :: Parser Integer
calculator = do
  i1  <- lexer tParser
  mOp <- optional (lexer $ L.oneOf "+-")
  case mOp of
    Just '+' -> (i1 +) <$> lexer tParser
    Just '-' -> (i1 -) <$> lexer tParser
    _        -> pure i1

tParser :: Parser Integer
tParser = do
  i1  <- lexer literalParser
  mOp <- optional (lexer $ L.oneOf "*/")
  case mOp of
    Just '*' -> (i1 *) <$> lexer literalParser
    Just '/' -> (i1 `div`) <$> lexer literalParser
    _        -> pure i1

literalParser :: Parser Integer
literalParser = prune >> choice [L.signed L.digits, parens (lexer (L.char '(')) (lexer (L.char ')')) calculator]
