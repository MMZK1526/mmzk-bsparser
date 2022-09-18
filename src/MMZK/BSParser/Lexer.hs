-- Utility lexers for characters, strings and digits.

module MMZK.BSParser.Lexer where

import           Control.Monad
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


--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

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

-- | Parse a string of (unsigned) digits. The result is a "String".
digitsStr :: Monad m => BSParserT e m String
digitsStr = some digitChar
{-# INLINE [2] digitsStr #-}

-- | Parse a string of (unsigned) hex digits. The result is a "String".
hexDigitsStr :: Monad m => BSParserT e m String
hexDigitsStr = some hexDigitChar
{-# INLINE [2] hexDigitsStr #-}

-- | Parse a string of (unsigned) oct digits. The result is a "String".
octDigitsStr :: Monad m => BSParserT e m String
octDigitsStr = some octDigitChar
{-# INLINE [2] octDigitsStr #-}

-- | Parse a string of (unsigned) bin digits. The result is a "String".
binDigitsStr :: Monad m => BSParserT e m String
binDigitsStr = some binDigitChar
{-# INLINE [2] binDigitsStr #-}

-- | Parse a string of alphabetic Unicode characters, following their General
-- Categories.
alphas :: Monad m => BSParserT e m String
alphas = some alpha
{-# INLINE [2] alphas #-}

-- | Parse a string of alphabetic Unicode characters or digits.
alphaDigits :: Monad m => BSParserT e m String
alphaDigits = some alphaDigit
{-# INLINE [2] alphaDigits #-}

-- | Parse a str Categories.
alphaNums :: Monad m => BSParserT e m String
alphaNums = some alphaNum
{-# INLINE [2] alphaNums #-}

-- | Parse a string of alphabetic Unicode characters, digits, or underscores,
-- but not starting with a digit.
identifier :: Monad m => BSParserT e m String
identifier = liftM2 (:) (satisfy (\ch -> isAlpha ch || ch == '_'))
           $ many (satisfy (\ch -> isAlpha ch || isDigit ch || ch == '_'))
{-# INLINE [2] identifier #-}


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

-- | Parse a single digitChar: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digitChar :: Monad m => BSParserT e m Char
digitChar = satisfy isDigit <?> [bilDigit defaultBuiltInLabels]
{-# INLINE [2] digitChar #-}

-- | Parse a single hexadecimal digitChar: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F.
hexDigitChar :: Monad m => BSParserT e m Char
hexDigitChar = satisfy isHexDigit <?> [bilHexDigit defaultBuiltInLabels]
{-# INLINE [2] hexDigitChar #-}

-- | Parse a single octacal digitChar: 0, 1, 2, 3, 4, 5, 6, or 7.
octDigitChar :: Monad m => BSParserT e m Char
octDigitChar = satisfy isOctDigit <?> [bilOctDigit defaultBuiltInLabels]
{-# INLINE [2] octDigitChar #-}

-- | Parse a single binary digitChar: 0 or 1.
binDigitChar :: Monad m => BSParserT e m Char
binDigitChar = satisfy (`elem` "01")
      <??> M.singleton Nothing (S.fromList $ CToken <$> "01")
{-# INLINE [2] binDigitChar #-}

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
-- digitChar: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
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


--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

-- | Parse a char of single digit into number.
digit :: Num a => Monad m => BSParserT e m a
digit = fromIntegral . digitToInt <$> digitChar
{-# INLINE [2] digit #-}

-- | Parse a char of single hex digit into number.
hexDigit :: Num a => Monad m => BSParserT e m a
hexDigit = fromIntegral . digitToInt <$> hexDigitChar
{-# INLINE [2] hexDigit #-}

-- | Parse a char of single oct digits into number.
octDigit :: Num a => Monad m => BSParserT e m a
octDigit = fromIntegral . digitToInt <$> octDigitChar
{-# INLINE [2] octDigit #-}

-- | Parse a char of single bin digits into number.
binDigit :: Num a => Monad m => BSParserT e m a
binDigit = fromIntegral . digitToInt <$> binDigitChar
{-# INLINE [2] binDigit #-}

-- | Parse a string of (unsigned) digits into number.
digits :: Num a => Monad m => BSParserT e m a
digits = foldl' ((+) . (* 10)) 0
       . fmap (fromIntegral . digitToInt) <$> some digitChar
{-# INLINE [2] digits #-}

-- | Parse a string of (unsigned) hex digits into number.
hexDigits :: Num a => Monad m => BSParserT e m a
hexDigits = foldl' ((+) . (* 16)) 0
          . fmap (fromIntegral . digitToInt) <$> some hexDigitChar
{-# INLINE [2] hexDigits #-}

-- | Parse a string of (unsigned) oct digits into number.
octDigits :: Num a => Monad m => BSParserT e m a
octDigits = foldl' ((+) . (* 8)) 0
          . fmap (fromIntegral . digitToInt) <$> some octDigitChar
{-# INLINE [2] octDigits #-}

-- | Parse a string of (unsigned) bin digits into number.
binDigits :: Num a => Monad m => BSParserT e m a
binDigits = foldl' ((+) . (* 2)) 0
          . fmap (fromIntegral . digitToInt) <$> some binDigitChar
{-# INLINE [2] binDigits #-}

-- | Parses a string of digits into a pure decimal number (i.e. "114514" to
-- 0.114514). Does not take the decimal point.
fractional :: Fractional a => Monad m => BSParserT e m a
fractional = foldl' ((. (/ 10)) . (+)) 0
           . fmap (fromIntegral . digitToInt) <$> some digitChar
{-# INLINE [2] fractional #-}

-- | Parses a string of hex digits into a pure decimal number (i.e. "114514" to
-- 0.114514). Does not take the decimal point.
hexFractional :: Fractional a => Monad m => BSParserT e m a
hexFractional = foldl' ((. (/ 10)) . (+)) 0
              . fmap (fromIntegral . digitToInt) <$> some hexDigitChar
{-# INLINE [2] hexFractional #-}

-- | Parses a string of oct digits into a pure decimal number (i.e. "114514" to
-- 0.114514). Does not take the decimal point.
octFractional :: Fractional a => Monad m => BSParserT e m a
octFractional = foldl' ((. (/ 10)) . (+)) 0
              . fmap (fromIntegral . digitToInt) <$> some octDigitChar
{-# INLINE [2] octFractional #-}

-- | Parses a string of bin digits into a pure decimal number (i.e. "10101" to
-- 0.10101). Does not take the decimal point.
binFractional :: Fractional a => Monad m => BSParserT e m a
binFractional = foldl' ((. (/ 10)) . (+)) 0
              . fmap (fromIntegral . digitToInt) <$> some binDigitChar
{-# INLINE [2] binFractional #-}

-- | Take a "BSParserT" that parses a number, apply it with the sign ('+', '-'
-- or nothing).
signed :: Num a => Monad m => BSParserT e m a -> BSParserT e m a
signed = ap (asum [id <$ char '+', negate <$ char '-', pure id])
{-# INLINE [2] signed #-}

-- | Take two parsers for the integral part and decimal part (does not include
-- the decimal point) of a number, combining them together.
decimate :: Fractional a => Monad m
         => BSParserT e m a -> BSParserT e m a -> BSParserT e m a
decimate = (. (char '.' >>)) . liftM2 (+)
{-# INLINE [2] decimate #-}

-- | Take the base and two parsers for the coefficient part and exponential part
-- of a number,
-- combining them together. The exponential part must be an integer.
scientify :: Fractional a => Integral b => Monad m
          => Int -> BSParserT e m a -> BSParserT e m b -> BSParserT e m a
scientify b pC pE = liftM2 ((. (fromIntegral b ^^)) . (*)) pC (oneOf "eE" >> pE)
{-# INLINE [2] scientify #-}

-- | Parses an (unsigned) real number.
-- Allows not having an integral part (i.e. starting with the decimal point).
-- Allows not having a decimal point providing an integral part is present.
-- Allows having a decimal point but not a decimal part (e.g. "3.").
-- Allows scientific notation (e.g. followed by "e12" or "E-1").
real :: Fractional a => Monad m => BSParserT e m a
real = do
  intPart <- optional digits
  coePart <- case intPart of
    Nothing -> decimate (pure 0) fractional
    Just n  -> asum [decimate (pure n) (asum [fractional, pure 0]), pure n]
  asum [ scientify 10 (pure coePart) (asum [signed digits, pure (0 :: Int)])
       , pure coePart ]
