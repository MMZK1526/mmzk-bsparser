import           Data.Char
import           MMZK.BSParser
import           MMZK.BSParser.Expr
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

calculator :: Parser Integer
calculator = mkExpr (choice [ lexer $ L.signed L.digits
                            , parens (lexer $ L.char '(') (lexer $ L.char ')') 
                                     calculator ]) 
                    [ [ infixL (+) (lexer $ L.char '+')
                      , infixL (-) (lexer $ L.char '-') ]
                    , [ infixL (*) (lexer $ L.char '*')
                      , infixL div (lexer $ L.char '/') ]
                    , [ prefix succ (lexer $ L.string "++")
                      , prefix pred (lexer $ L.string "--")
                      , postfix succ (lexer $ L.string "++")
                      , postfix pred (lexer $ L.string "--") ] ]
