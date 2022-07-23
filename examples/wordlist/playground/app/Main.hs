import           Data.Char
import           MMZK.BSParser
import qualified MMZK.BSParser.Lexer as L
import qualified MMZK.BSParser.CPS as CPS

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

test :: Show a => Parser a -> String -> IO ()
test parser inputStr = putStrLn 
                     $ case parse (L.wrapper (many L.space) parser) inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err

letterParser :: Parser Char
letterParser = L.satisfy (\ch -> isAlpha ch && isAscii ch) <?> ["ascii letter"]

wordParser :: Parser String
wordParser = ( CPS.some letterParser
             . CPS.manyS ( (prune >>)
                         . CPS.cons (choice [L.char '-', L.char '\''])
                         . CPS.some letterParser ) ) (pure [])

wordlistParser :: Parser [String]
wordlistParser = sepBy1 (lexer $ L.char ',') (prune >> lexer wordParser)
