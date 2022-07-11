-- Utility parsers for single 8-bit ASCII and Latin-1 "Char"s.

module MMZKBSParser.ASCII where

import           MMZKBSParser.Convert
import           MMZKBSParser.Parser
import qualified MMZKBSParser.Word8 as P8


--------------------------------------------------------------------------------
-- ASCII & Latin-1
--------------------------------------------------------------------------------

-- | Parse the given ASCII or Latin-1 "Char".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
char :: Monad m => Char -> ParserT m Char
char = satisfyL1 . (==)
{-# INLINE char #-}

-- | Parse one of the given ASCII or Latin-1 "Char".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
oneOf :: Monad m => Foldable t => t Char -> ParserT m Char
oneOf chs = satisfyL1 (`elem` chs)
{-# INLINE oneOf #-}

-- | Parse the SPACE character (空格), namely 32.
space32 :: Monad m => ParserT m Char
space32 = toChar <$> P8.space32
{-# INLINE space32 #-}

-- | Parse a space character (空白字符), namely \t, \n, \r, \f, \v, or 32.
space :: Monad m => ParserT m Char
space = toChar <$> P8.space
{-# INLINE space #-}

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digit :: Monad m => ParserT m Char
digit = toChar <$> P8.digit
{-# INLINE digit #-}

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F.
hexDigit :: Monad m => ParserT m Char
hexDigit = toChar <$> P8.hexDigit
{-# INLINE hexDigit #-}

-- | Parse a single octacal digit: 0, 1, 2, 3, 4, 5, 6, or 7.
octDigit :: Monad m => ParserT m Char
octDigit = toChar <$> P8.octDigit
{-# INLINE octDigit #-}

-- | Parse a single binary digit: 0 or 1.
binDigit :: Monad m => ParserT m Char
binDigit = toChar <$> P8.binDigit
{-# INLINE binDigit #-}


--------------------------------------------------------------------------------
-- ASCII
--------------------------------------------------------------------------------

-- | Parse one ASCII "Char" using the predicate. The function would reject any
-- non-ASCII "Char".
satisfy :: Monad m => (Char -> Bool) -> ParserT m Char
satisfy f = toChar <$> P8.satisfy (f . toChar)
{-# INLINE satisfy #-}

-- | Parse any single ASCII "Char" except those in the list.
noneOf :: Monad m => Foldable t => t Char -> ParserT m Char
noneOf chs = satisfy (`notElem` chs)
{-# INLINE noneOf #-}

-- | Parse an ASCII character, namely anything below 128.
ascii :: Monad m => ParserT m Char
ascii = toChar <$> P8.ascii
{-# INLINE ascii #-}

-- | Parse a single ASCII letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z.
alpha :: Monad m => ParserT m Char
alpha = toChar <$> P8.alpha
{-# INLINE alpha #-}

-- | Parse a single ASCII letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
alphaNum :: Monad m => ParserT m Char
alphaNum = toChar <$> P8.alphaNum
{-# INLINE alphaNum #-}

-- | Parse a single lowercase ASCII letter: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, or z.
lower :: Monad m => ParserT m Char
lower = toChar <$> P8.lower
{-# INLINE lower #-}

-- | Parse a single uppercase ASCII letter: A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, or Z.
upper :: Monad m => ParserT m Char
upper = toChar <$> P8.upper
{-# INLINE upper #-}


--------------------------------------------------------------------------------
-- Latin-1
--------------------------------------------------------------------------------

-- | Parse one Latin-1 "Char" using the predicate. The function would reject any
-- non-Latin-1 "Char".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
satisfyL1 :: Monad m => (Char -> Bool) -> ParserT m Char
satisfyL1 f = toChar <$> P8.satisfy (f . toChar)
{-# INLINE satisfyL1 #-}

-- | Parse any single Latin-1 "Char" except those in the list.
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
noneOfL1 :: Monad m => Foldable t => t Char -> ParserT m Char
noneOfL1 chs = satisfyL1 (`notElem` chs)
{-# INLINE noneOfL1 #-}

-- | Parse any single 8-bit Latin-1 "Char".
latin1 :: Monad m => ParserT m Char
latin1 = toChar <$> P8.latin1
{-# INLINE latin1 #-}

-- | Parse a single Latin-1 letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê, Ë, Ì, Í, Î,
-- Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; à, á, â, ã, ä, å, æ, ç, è, é, ê, ë,
-- ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; ª, µ, º, Þ, ß, þ, ÿ.
alphaL1 :: Monad m => ParserT m Char
alphaL1 = toChar <$> P8.alphaL1
{-# INLINE alphaL1 #-}

-- | Parse a single Latin-1 letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê, Ë,
-- Ì, Í, Î, Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; à, á, â, ã, ä, å, æ, ç, è,
-- é, ê, ë, ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; ª, µ, º, Þ, ß, þ,
-- ÿ; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
alphaNumL1 :: Monad m => ParserT m Char
alphaNumL1 = toChar <$> P8.alphaNumL1
{-# INLINE alphaNumL1 #-}

-- | Parse a single lowercase Latin-1 letter: a, b, c, d, e, f, g, h, i, j, k,
-- l, m, n, o, p, q, r, s, t, u, v, w, x, y, z; à, á, â, ã, ä, å, æ, ç, è, é, ê,
-- ë, ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; µ, ß, ÿ.
lowerL1 :: Monad m => ParserT m Char
lowerL1 = toChar <$> P8.lowerL1
{-# INLINE lowerL1 #-}

-- | Parse a single uppercase Latin-1 letter: A, B, C, D, E, F, G, H, I, J, K,
-- L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê,
-- Ë, Ì, Í, Î, Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; Þ.
upperL1 :: Monad m => ParserT m Char
upperL1 = toChar <$> P8.upperL1
{-# INLINE upperL1 #-}
