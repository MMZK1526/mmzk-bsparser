-- Utility lexers for characters, strings and digits.

module MMZK.BSParser.Lexer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Error
import           MMZK.BSParser.Parser

-- | Use the "BSParserT", then consume the spaces that follows. Spaces are
-- defined by "spaceParser" of "ParseState", which is by default @pure ()@.
lexer :: Monad m => BSParserT e m a -> BSParserT e m a
lexer = (<* (getState >>= spaceParser))
{-# INLINE [2] lexer #-}

-- | Set "spaceParser", use it to ignore leading spaces, run the "BSParserT",
-- and use "eof" to reject extraneous inputs.
wrapper :: Monad m
        => BSParserT e m b -> BSParserT e m a -> BSParserT e m a
wrapper bp p = setSpaceParser bp >> bp >> p <* eof
{-# INLINE [2] wrapper #-}


--------------------------------------------------------------------------------
-- String
--------------------------------------------------------------------------------

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "ByteString".
byteString :: Monad m => ByteStringLike s => s -> BSParserT e m ByteString
byteString str = tokens (BS.length bs) 
                        (\bs' -> if bs == bs' then Just bs else Nothing)
            <??> M.singleton Nothing (S.singleton $ SToken $ convert str)
  where
    convert = fromByteString . toByteString
    bs      = toByteString str
{-# INLINE [2] byteString #-}

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "String".
string :: Monad m => ByteStringLike s => s -> BSParserT e m String
string = fmap fromByteString . byteString
{-# INLINE [2] string #-}

-- | Parse the given "ByteStringLike", including "String". The result is a
-- "Text".
text :: Monad m => ByteStringLike s => s -> BSParserT e m Text
text = fmap fromByteString . byteString
{-# INLINE [2] text #-}


--------------------------------------------------------------------------------
-- Char
--------------------------------------------------------------------------------

-- | Parse the given "Char".
char :: Monad m => Char -> BSParserT e m Char
char ch = charToken (\x -> if x == ch then Just x else Nothing)
     <??> M.singleton Nothing (S.singleton $ CToken ch)
{-# INLINE [2] char #-}

-- | Parse one "Char" using the predicate.
satisfy :: Monad m => (Char -> Bool) -> BSParserT e m Char
satisfy f = charToken $ \x -> if f x then Just x else Nothing
{-# INLINE [2] satisfy #-}

-- | Parse any "Char".
anyChar :: Monad m => BSParserT e m Char
anyChar = charToken Just
{-# INLINE [2] anyChar #-}

-- | Succeed iff the "BSParserT" fails. Will not consume any input or modify any
-- state.
neg :: Monad m => BSParserT e m a -> BSParserT e m ()
neg p = do
  snapshot <- inspect $ lookAhead p
  case snapshot of
    Left err -> case esError err of
      BadUTF8 -> throw err
      _       -> pure ()
    Right _  -> do
      ps      <- getState
      (i, ch) <- lookAhead $ withLen anyChar
      throw . ErrSpan (parseIndex ps, parseIndex ps + i - 1)
            $ BasicErr (UItem Nothing (Just $ CToken ch)) M.empty []
{-# INLINE [2] neg #-}

-- | Parse the end-of-input.
eof :: Monad m => BSParserT e m ()
eof = do
  snapshot <- inspect . lookAhead $ withLen anyChar
  case snapshot of
    Left err      -> case esError err of
      BadUTF8 -> throw err
      _       -> pure ()
    Right (i, ch) -> do
      ps <- getState
      throw . ErrSpan (parseIndex ps, parseIndex ps + i - 1)
            $ BasicErr (UItem Nothing (Just $ CToken ch))
                       (M.singleton Nothing (S.singleton EOI)) []
{-# INLINE [2] eof #-}

-- | Parse one ASCII character, namely anything with a codepoint below 128.
ascii :: Monad m => BSParserT e m Char
ascii = satisfy isAscii <?> [bilAscii defaultBuiltInLabels]
{-# INLINE [2] ascii #-}

-- | Parse one of the given "Char"s.
oneOf :: Monad m => Foldable t => t Char -> BSParserT e m Char
oneOf chs = satisfy (`elem` chs)
       <??> M.singleton Nothing (S.fromList $ CToken <$> toList chs)
{-# INLINE [2] oneOf #-}

-- | Parse any Char except those in the list.
noneOf :: Monad m => Foldable t => t Char -> BSParserT e m Char
noneOf chs = satisfy (`notElem` chs)
{-# INLINE [2] noneOf #-}

-- | Parse the SPACE character (空格), namely 32.
space32 :: Monad m => BSParserT e m Char
space32 = satisfy (== ' ') <??> M.singleton Nothing (S.singleton $ CToken ' ')
{-# INLINE [2] space32 #-}

-- | Parse a space character (空白字符), following their General Categories.
space :: Monad m => BSParserT e m Char
space = satisfy isSpace <?> [bilSpace defaultBuiltInLabels]
{-# INLINE [2] space #-}

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digit :: Monad m => BSParserT e m Char
digit = satisfy isDigit <?> [bilDigit defaultBuiltInLabels]
{-# INLINE [2] digit #-}

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F.
hexDigit :: Monad m => BSParserT e m Char
hexDigit = satisfy isHexDigit <?> [bilHexDigit defaultBuiltInLabels]
{-# INLINE [2] hexDigit #-}

-- | Parse a single octacal digit: 0, 1, 2, 3, 4, 5, 6, or 7.
octDigit :: Monad m => BSParserT e m Char
octDigit = satisfy isOctDigit <?> [bilOctDigit defaultBuiltInLabels]
{-# INLINE [2] octDigit #-}

-- | Parse a single binary digit: 0 or 1.
binDigit :: Monad m => BSParserT e m Char
binDigit = satisfy (`elem` "01")
      <??> M.singleton Nothing (S.fromList $ CToken <$> "01")
{-# INLINE [2] binDigit #-}

-- | Parse a single Unicode number character, following their General
-- Categories.
num :: Monad m => BSParserT e m Char
num = satisfy isNumber <?> [bilNum defaultBuiltInLabels]
{-# INLINE [2] num #-}

-- | Parse a single alphabetic Unicode characters, following their General
-- Categories, including Latin, Cyrillic, Greek, CJK, and the script of other
-- languages.
alpha :: Monad m => BSParserT e m Char
alpha = satisfy isAlpha <?> [bilAlpha defaultBuiltInLabels]
{-# INLINE [2] alpha #-}

-- | Parse a single Unicode letter (following their General Categories) or a
-- digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
alphaDigit :: Monad m => BSParserT e m Char
alphaDigit = satisfy (\ch -> isAlpha ch || isDigit ch)
         <?> [bilAlpha defaultBuiltInLabels, bilDigit defaultBuiltInLabels]
{-# INLINE [2] alphaDigit #-}

-- | Parse a single Unicode letter or number character, following their General
-- Categories.
alphaNum :: Monad m => BSParserT e m Char
alphaNum = satisfy isAlphaNum
         <?> [bilAlpha defaultBuiltInLabels, bilNum defaultBuiltInLabels]
{-# INLINE [2] alphaNum #-}

-- | Parse a single lowercase alphabetic Unicode characters, following their
-- General Categories.
lower :: Monad m => BSParserT e m Char
lower = satisfy isLower <?> [bilLower defaultBuiltInLabels]
{-# INLINE [2] lower #-}

-- | Parse a single uppercase alphabetic Unicode characters, following their
-- General Categories.
upper :: Monad m => BSParserT e m Char
upper = satisfy isUpper <?> [bilUpper defaultBuiltInLabels]
{-# INLINE [2] upper #-}
