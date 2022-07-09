-- Utility lexers for characters, strings and digits.

module MMZKBSParser.Lexer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           MMZKBSParser.Convert
import           MMZKBSParser.Parser

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "ByteString".
string :: Monad m => ByteStringLike s => s -> ParserT m ByteString
string str 
  = tokens (BS.length bs) (\bs' -> if bs == bs' then Just bs else Nothing)
  where
    bs = toByteString str

-- | Parse the given "Char".
char :: Monad m => Char -> ParserT m Char
char = fmap (head . fromByteString) . string . (: [])
