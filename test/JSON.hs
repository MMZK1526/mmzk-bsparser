import           Base
import           Data.Char
import           MMZK.BSParser
import           MMZK.BSParser.Error
import qualified MMZK.BSParser.CPS as CPS
import qualified MMZK.BSParser.Lexer as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Test.HUnit

main :: IO ()
main = case parseJSON " [1, 3.2, true, \"\\u0020\", {1: null, \"b\": -2e3} ]" of
  Left x -> print $ renderErrBundle (x :: ErrBundle String)
  Right a -> print a

data JSON = Obj [(String, JSON)]
          | Arr [JSON]
          | Str String
          | Num Double
          | Lit Lit
  deriving (Eq, Ord, Show)

data Lit = JTrue | JFalse | JNull
  deriving (Eq, Ord, Show)

parseJSON :: ByteStringLike s => s -> Either (ErrBundle String) JSON
parseJSON = parse (L.wrapper jSpace jsonParser)

jsonParser :: Monad m => BSParserT e m JSON
jsonParser = prune >> choice [ Num <$> jNum
                             , Lit <$> jLit
                             , Str <$> jStr
                             , Arr <$> jArr
                             , Obj <$> jObj ]

-- | Parse JSON whitespaces.
jSpace :: Monad m => BSParserT e m ()
jSpace = void . many $ L.oneOf "\t\r\n "

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
        Just _  -> choice [ L.decimate (pure 0) L.fractional
                          , 0 <$ L.neg L.digitChar ]
        Nothing -> do
          intPart <- L.digits
          choice [L.decimate (pure intPart) L.fractional, pure intPart]
      choice [ L.scientify 10 (pure coePart) 
                              (choice [L.signed L.digits, pure 0])
             , pure coePart ]

-- | Parse "true", "false", and "null".
jLit :: Monad m => BSParserT e m Lit
jLit = pmap (`lookup` [("true", JTrue), ("false", JFalse), ("null", JNull)])
            L.alphas <?> ["JSON literal"]

-- | Parse a JSON array.
jArr :: Monad m => BSParserT e m [JSON]
jArr = CPS.runCPS . CPS.parens (lexer $ L.char '[') (lexer $ L.char ']')
     $ CPS.sepBy (lexer $ L.char ',') (L.lexer jsonParser)

-- | Parse a JSON string.
jStr :: Monad m => BSParserT e m String
jStr = L.char '"' <?> ["string literal"] >> inner
  where
    inner = do
      ch <- L.satisfy (not . isControl) <?> ["non-control character"]
      case ch of
        '"'  -> pure ""
        '\\' -> do
          esc <- pbind ( fromMaybe empty 
                       . (`lookup` [ ('"', pure '"')
                                   , ('\\', pure '\\')
                                   , ('/', pure '/')
                                   , ('b', pure '\b')
                                   , ('f', pure '\f')
                                   , ('n', pure '\n')
                                   , ('r', pure '\r')
                                   , ('t', pure '\t')
                                   , ('u', jCodepoint) ]) ) L.anyChar
             <?> ["Escape character"]
          (esc :) <$> inner
        _    -> (ch :) <$> inner
    jCodepoint = chr . foldl ((+) . (16 *)) 0 <$> replicateM 4 L.hexDigit

-- | Parse a JSON object.
jObj :: Monad m => BSParserT e m [(String, JSON)]
jObj = CPS.runCPS . CPS.parens (lexer $ L.char '{') (lexer $ L.char '}')
     . CPS.sepBy (lexer $ L.char ',') $ do
  prune
  key   <- lexer jStr
  lexer . void $ L.char ':'
  value <- lexer jsonParser
  return (key, value)
