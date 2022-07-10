-- Utility lexers for characters, strings and digits.

module MMZKBSParser.Lexer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
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

-- | Parse one "Char" using the predicate.
satisfy :: Monad m => (Char -> Bool) -> ParserT m Char
satisfy f = charToken $ \x -> if f x then Just x else Nothing

-- | Parse any "Char".
anyChar :: Monad m => ParserT m Char
anyChar = charToken Just

-- | Parse one of the given "Char"s.
oneOf :: Monad m => Foldable t => t Char -> ParserT m Char
oneOf chs = satisfy (`elem` chs)

-- | Parse any Char except those in the list.
noneOf :: Monad m => Foldable t => t Char -> ParserT m Char
noneOf chs = satisfy (`notElem` chs)

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digit :: Monad m => ParserT m Char
digit = satisfy isDigit
{-# INLINE digit #-}

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F. The result is retained as a "Word8".
hexDigit :: Monad m => ParserT m Char
hexDigit = satisfy isHexDigit
{-# INLINE hexDigit #-}

-- | Parse a single alphabetic Unicode characters (following their General
-- Categories), including Latin, Cyrillic, Greek, CJK, and the script of other
-- languages.
alpha :: Monad m => ParserT m Char
alpha = satisfy isAlpha
{-# INLINE alpha #-}

-- | Parse a single lowercase alphabetic Unicode characters (following their 
-- General Categories).
lower :: Monad m => ParserT m Char
lower = satisfy isLower
{-# INLINE lower #-}

-- | Parse a single uppercase alphabetic Unicode characters (following their 
-- General Categories).
upper :: Monad m => ParserT m Char
upper = satisfy isUpper
{-# INLINE upper #-}
