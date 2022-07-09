-- Utility parsers for single "Word8" tokens.

module MMZKBSParser.Word8 where

import           Data.Word
import           MMZKBSParser.Parser

-- | Parse one token ("Word8") using the predicate. The result is retained as
-- a "Word8".
satisfy :: Monad m => (Word8 -> Bool) -> ParserT m Word8
satisfy f = token $ \x -> if f x then Just x else Nothing

-- | Parse any token ("Word8"). The result is retained as a "Word8".
anyToken :: Monad m => ParserT m Word8
anyToken = token Just

-- | Parse one of the given tokens ("Word8"). The result is retained as a
-- "Word8".
-- Use it with care if the list of tokens contains NON-ASCII codepoints since it
-- may extract a fragment of a codepoint.
oneOf :: Monad m => Foldable t => t Word8 -> ParserT m Word8
oneOf chs = satisfy (`elem` chs)

-- | Parse any token ("Word8") except those in the list. The result is retained
-- as a "Word8".
-- Use it with extreme care since it may extract a fragment of a codepoint if
-- the input "ByteString" contains non-ASCII characters.
noneOf :: Monad m => Foldable t => t Word8 -> ParserT m Word8
noneOf chs = satisfy (`notElem` chs)

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9. The result is
-- retained as a "Word8".
digit :: Monad m => ParserT m Word8
digit = satisfy (\ch -> ch >= 48 && ch <= 57)

-- | Parse a single Latin letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z. The result is retained as a "Word8".
alpha :: Monad m => ParserT m Word8
alpha = satisfy (\ch -> (ch >= 65 && ch <= 90) || (ch >= 97 && ch <= 122))

-- | Parse a single Latin letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. The
-- result is retained as a "Word8".
alphaNum :: Monad m => ParserT m Word8
alphaNum = satisfy ( \ch -> (ch >= 48 && ch <= 57) 
                         || (ch >= 65 && ch <= 90) 
                         || (ch >= 97 && ch <= 122) )

-- | Parse a single lowercase Latin letter: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, or z. The result is retained as a
-- "Word8".
lower :: Monad m => ParserT m Word8
lower = satisfy (\ch -> ch >= 97 && ch <= 122)

-- | Parse a single uppercase Latin letter: A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, or Z. The result is retained as a
-- "Word8".
upper :: Monad m => ParserT m Word8
upper = satisfy (\ch -> ch >= 65 && ch <= 90)
