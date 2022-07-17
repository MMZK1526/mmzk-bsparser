-- Utility lexers for characters, strings and digits.

module MMZK.BSParser.Lexer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Text (Text)
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Parser

-- | Use the "ParserT", then consume the spaces that follows. Spaces are defined
-- by "spaceParser" of "ParseState", which is by default @pure ()@.
lexer :: Monad m => ParserT m a -> ParserT m a
lexer = (<* (inspect >>= spaceParser))
{-# INLINE lexer #-}


--------------------------------------------------------------------------------
-- String
--------------------------------------------------------------------------------

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "ByteString".
byteString :: Monad m => ByteStringLike s => s -> ParserT m ByteString
byteString str 
  = tokens (BS.length bs) (\bs' -> if bs == bs' then Just bs else Nothing)
  where
    bs = toByteString str

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "String".
string :: Monad m => ByteStringLike s => s -> ParserT m String
string = fmap fromByteString . byteString

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "Text".
text :: Monad m => ByteStringLike s => s -> ParserT m Text
text = fmap fromByteString . byteString


--------------------------------------------------------------------------------
-- Char
--------------------------------------------------------------------------------

-- | Parse the given "Char".
char :: Monad m => Char -> ParserT m Char
char ch = charToken $ \x -> if x == ch then Just x else Nothing

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

-- | Parse the SPACE character (空格), namely 32.
space32 :: Monad m => ParserT m Char
space32 = satisfy (== ' ')
{-# INLINE space32 #-}

-- | Parse a space character (空白字符), following their General Categories.
space :: Monad m => ParserT m Char
space = satisfy isSpace
{-# INLINE space #-}

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digit :: Monad m => ParserT m Char
digit = satisfy isDigit
{-# INLINE digit #-}

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F.
hexDigit :: Monad m => ParserT m Char
hexDigit = satisfy isHexDigit
{-# INLINE hexDigit #-}

-- | Parse a single octacal digit: 0, 1, 2, 3, 4, 5, 6, or 7.
octDigit :: Monad m => ParserT m Char
octDigit = satisfy isOctDigit
{-# INLINE octDigit #-}

-- | Parse a single binary digit: 0 or 1.
binDigit :: Monad m => ParserT m Char
binDigit = satisfy (`elem` "01")
{-# INLINE binDigit #-}

-- | Parse a single alphabetic Unicode characters, following their General
-- Categories, including Latin, Cyrillic, Greek, CJK, and the script of other
-- languages.
alpha :: Monad m => ParserT m Char
alpha = satisfy isAlpha
{-# INLINE alpha #-}

-- | Parse a single ASCII letter (following their General Categories) or a
-- digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
alphaDigit :: Monad m => ParserT m Char
alphaDigit = satisfy (\ch -> isAlpha ch || isDigit ch)
{-# INLINE alphaDigit #-}

-- | Parse a single ASCII letter or number character, following their General
-- Categories.
alphaNum :: Monad m => ParserT m Char
alphaNum = satisfy isAlphaNum
{-# INLINE alphaNum #-}

-- | Parse a single lowercase alphabetic Unicode characters, following their 
-- General Categories.
lower :: Monad m => ParserT m Char
lower = satisfy isLower
{-# INLINE lower #-}

-- | Parse a single uppercase alphabetic Unicode characters, following their 
-- General Categories.
upper :: Monad m => ParserT m Char
upper = satisfy isUpper
{-# INLINE upper #-}
