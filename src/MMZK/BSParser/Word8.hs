-- Utility parsers for single "Word8" ASCII and Latin-1 tokens. Not recommended
-- unless the input stream is fully comprised of ASCII or Latin-1 encoding.

module MMZK.BSParser.Word8 where

import           Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Error
import           MMZK.BSParser.Parser


--------------------------------------------------------------------------------
-- ASCII & Latin-1
--------------------------------------------------------------------------------

-- | Parse the given token ("Word8"). The result is retained as a "Word8".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
char :: Monad m => Word8 -> BSParserT e m Word8
char ch = satisfyL1 (== ch) <??> M.singleton Nothing
                                            (S.singleton . CToken $ toChar ch)
{-# INLINE char #-}

-- | Succeed iff the "BSParserT" fails. Will not consume any input or modify any
-- state.
neg :: Monad m => BSParserT e m a -> BSParserT e m ()
neg p = do
  mR <- optional $ lookAhead p
  case mR of
    Nothing -> pure ()
    Just _  -> do
      ps      <- getState
      (i, ch) <- lookAhead $ withLen latin1
      throw . ErrSpan (parseIndex ps) i
            $ BasicErr (Just $ UItem Nothing (Just . CToken $ toChar ch))
                       Nothing Nothing
{-# INLINE neg #-}

-- | Parse the end-of-input.
eof :: Monad m => BSParserT e m ()
eof = do
  mR <- optional . lookAhead $ withLen latin1
  case mR of
    Nothing      -> pure ()
    Just (i, ch) -> do
      ps <- getState
      throw . ErrSpan (parseIndex ps) i
            $ BasicErr (Just $ UItem Nothing (Just . CToken $ toChar ch))
                       (Just $ M.singleton Nothing (S.singleton EOI)) Nothing
{-# INLINE eof #-}

-- | Parse one of the given tokens ("Word8"). The result is retained as a
-- "Word8".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
oneOf :: Monad m => Foldable t => t Word8 -> BSParserT e m Word8
oneOf chs = satisfyL1 (`elem` chs) 
       <??> M.singleton Nothing (S.fromList $ CToken . toChar <$> toList chs)
{-# INLINE oneOf #-}

-- | Parse a token ("Word8") representing the SPACE character (空格), namely 32.
-- The result is retained as a "Word8".
space32 :: Monad m => BSParserT e m Word8
space32 = satisfyL1 (== 32) <??> M.singleton Nothing (S.singleton $ CToken ' ')
{-# INLINE space32 #-}

-- | Parse a token ("Word8") representing a space character (空白字符), namely
-- \t, \n, \r, \f, \v, or  32. The result is retained as a "Word8".
space :: Monad m => BSParserT e m Word8
space = satisfyL1 (\ch -> ch == 32 || (ch >= 9 && ch <= 13)) <?> "space"
{-# INLINE space #-}

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9. The result is
-- retained as a "Word8".
digit :: Monad m => BSParserT e m Word8
digit = satisfyL1 (\ch -> ch >= 48 && ch <= 57) <?> "digit"
{-# INLINE digit #-}

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F. The result is retained as a "Word8".
hexDigit :: Monad m => BSParserT e m Word8
hexDigit = satisfyL1 ( \ch -> (ch >= 48 && ch <= 57)
                           || (ch >= 65 && ch <= 70)
                           || (ch >= 97 && ch <= 102) ) <?> "hex digit"
{-# INLINE hexDigit #-}

-- | Parse a single octacal digit: 0, 1, 2, 3, 4, 5, 6, or 7. The result is
-- retained as a "Word8".
octDigit :: Monad m => BSParserT e m Word8
octDigit = satisfyL1 (\ch -> ch >= 48 && ch <= 55) <?> "oct digit"
{-# INLINE octDigit #-}

-- | Parse a single binary digit: 0 or 1. The result is retained as a "Word8".
binDigit :: Monad m => BSParserT e m Word8
binDigit = satisfyL1 (\ch -> ch == 48 || ch == 49)
      <??> M.singleton Nothing (S.fromList $ CToken <$> "01")
{-# INLINE binDigit #-}


--------------------------------------------------------------------------------
-- ASCII Specific
--------------------------------------------------------------------------------

-- | Parse one token ("Word8") representing an ASCII character using the
-- predicate. The result is retained as a "Word8".
satisfy :: Monad m => (Word8 -> Bool) -> BSParserT e m Word8
satisfy f = token $ \x -> if f x && x < 128 then Just x else Nothing
{-# INLINE satisfy #-}

-- | Parse any token ("Word8") representing an ASCII character except those in
-- the list. The result is retained as a "Word8".
noneOf :: Monad m => Foldable t => t Word8 -> BSParserT e m Word8
noneOf chs = satisfy (`notElem` chs)
{-# INLINE noneOf #-}

-- | Parse one token ("Word8") representing an ASCII character, namely anything
-- below 128. The result is retained as a "Word8".
ascii :: Monad m => BSParserT e m Word8
ascii = satisfy $ const True
{-# INLINE ascii #-}

-- | Parse a single ASCII letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z. The result is retained as a "Word8".
alpha :: Monad m => BSParserT e m Word8
alpha = satisfyL1 (\ch -> (ch >= 65 && ch <= 90) || (ch >= 97 && ch <= 122))
{-# INLINE alpha #-}

-- | Parse a single ASCII letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. The
-- result is retained as a "Word8".
alphaNum :: Monad m => BSParserT e m Word8
alphaNum = satisfyL1 ( \ch -> (ch >= 48 && ch <= 57)
                           || (ch >= 65 && ch <= 90)
                           || (ch >= 97 && ch <= 122) )
{-# INLINE alphaNum #-}

-- | Parse a single lowercase ASCII letter: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, or z; The result is retained as a
-- "Word8".
lower :: Monad m => BSParserT e m Word8
lower = satisfyL1 (\ch -> ch >= 97 && ch <= 122)
{-# INLINE lower #-}

-- | Parse a single uppercase ASCII letter: A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, or Z. The result is retained as a
-- "Word8".
upper :: Monad m => BSParserT e m Word8
upper = satisfyL1 (\ch -> ch >= 65 && ch <= 90)
{-# INLINE upper #-}


--------------------------------------------------------------------------------
-- Latin-1 Specific
--------------------------------------------------------------------------------

-- | Parse one token ("Word8") representing an ASCII character using the
-- predicate. The result is retained as a "Word8".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
satisfyL1 :: Monad m => (Word8 -> Bool) -> BSParserT e m Word8
satisfyL1 f = token $ \x -> if f x then Just x else Nothing
{-# INLINE satisfyL1 #-}

-- | Parse any token ("Word8") except those in the list. The result is retained
-- as a "Word8".
-- Use it with care if the list of tokens contains non-Latin-1 codepoints since
-- it may extract a fragment of a codepoint.
noneOfL1 :: Monad m => Foldable t => t Word8 -> BSParserT e m Word8
noneOfL1 chs = satisfy (`notElem` chs)
{-# INLINE noneOfL1 #-}

-- | Parse any token ("Word8"). The result is retained as a "Word8".
latin1 :: Monad m => BSParserT e m Word8
latin1 = token Just
{-# INLINE latin1 #-}

-- | Parse a single Latin-1 letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê, Ë, Ì, Í, Î,
-- Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; à, á, â, ã, ä, å, æ, ç, è, é, ê, ë,
-- ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; ª, µ, º, Þ, ß, þ, ÿ. The
-- result is retained as a "Word8".
alphaL1 :: Monad m => BSParserT e m Word8
alphaL1 = satisfyL1 ( \ch -> (ch >= 65 && ch <= 90)
                 || (ch >= 97 && ch <= 122)
                 || ch == 170 || ch == 181 || ch == 186
                 || (ch >= 192 && ch /= 215 && ch /= 247) )
{-# INLINE alphaL1 #-}

-- | Parse a single Latin-1 letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê, Ë,
-- Ì, Í, Î, Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; à, á, â, ã, ä, å, æ, ç, è,
-- é, ê, ë, ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; ª, µ, º, Þ, ß, þ,
-- ÿ; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. The result is retained as a "Word8".
alphaNumL1 :: Monad m => BSParserT e m Word8
alphaNumL1 = satisfyL1 ( \ch -> (ch >= 48 && ch <= 57)
                             || (ch >= 65 && ch <= 90)
                             || (ch >= 97 && ch <= 122)
                             || ch == 170 || ch == 181 || ch == 186
                             || (ch >= 192 && ch /= 215 && ch /= 247) )
{-# INLINE alphaNumL1 #-}

-- | Parse a single lowercase Latin-1 letter: a, b, c, d, e, f, g, h, i, j, k,
-- l, m, n, o, p, q, r, s, t, u, v, w, x, y, z; à, á, â, ã, ä, å, æ, ç, è, é, ê,
-- ë, ì, í, î, ï, ð, ñ, ò, ó, ô, õ, ö, ø, ù, ú, û, ü, ý; µ, ß, ÿ. The result is
-- retained as a "Word8".
lowerL1 :: Monad m => BSParserT e m Word8
lowerL1 = satisfyL1 ( \ch -> (ch >= 97 && ch <= 122)
                          || ch == 181
                          || (ch >= 223 && ch /= 247) )
{-# INLINE lowerL1 #-}

-- | Parse a single uppercase Latin-1 letter: A, B, C, D, E, F, G, H, I, J, K,
-- L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; À, Á, Â, Ã, Ä, Å, Æ, Ç, È, É, Ê,
-- Ë, Ì, Í, Î, Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, Ý; Þ. The result is
-- retained as a "Word8".
upperL1 :: Monad m => BSParserT e m Word8
upperL1 = satisfyL1 ( \ch -> (ch >= 65 && ch <= 90) 
                          || (ch >= 192 && ch <= 222 && ch /= 215) )
{-# INLINE upperL1 #-}
