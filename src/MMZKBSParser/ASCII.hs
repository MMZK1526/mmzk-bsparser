-- Utility parsers for single 8-bit ASCII "Char"s.

module MMZKBSParser.ASCII where

import           MMZKBSParser.Convert
import           MMZKBSParser.Parser
import qualified MMZKBSParser.Word8 as P8

-- | Parse one ASCII "Char" using the predicate. The function would reject any
-- non-ASCII char.
satisfy :: Monad m => (Char -> Bool) -> ParserT m Char
satisfy f = toChar <$> P8.satisfy (f . toChar)

-- | Parse any single 8-bit "Char".
anyToken :: Monad m => ParserT m Char
anyToken = toChar <$> P8.anyToken

-- | Parse one of the given 8-bit "Char".
oneOf :: Monad m => Foldable t => t Char -> ParserT m Char
oneOf chs = satisfy (`elem` chs)

-- | Parse any 8-bit "Char" except those in the list.
-- Use it with extreme care since it may extract a fragment of a codepoint if
-- the input "ByteString" contains non-ASCII characters.
noneOf :: Monad m => Foldable t => t Char -> ParserT m Char
noneOf chs = satisfy (`notElem` chs)

-- | Parse a single digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.
digit :: Monad m => ParserT m Char
digit = toChar <$> P8.digit

-- | Parse a single hexadecimal digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9; a, b, c, d,
-- e, f; A, B, C, D, E, F. The result is retained as a "Word8".
hexDigit :: Monad m => ParserT m Char
hexDigit = toChar <$> P8.hexDigit

-- | Parse a single Latin letter: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
-- p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
-- P, Q, R, S, T, U, V, W, X, Y, Z.
alpha :: Monad m => ParserT m Char
alpha = toChar <$> P8.alpha

-- | Parse a single Latin letter or digit: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, z; A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
alphaNum :: Monad m => ParserT m Char
alphaNum = toChar <$> P8.alphaNum

-- | Parse a single lowercase Latin letter: a, b, c, d, e, f, g, h, i, j, k, l,
-- m, n, o, p, q, r, s, t, u, v, w, x, y, or z.
lower :: Monad m => ParserT m Char
lower = toChar <$> P8.lower

-- | Parse a single uppercase Latin letter: A, B, C, D, E, F, G, H, I, J, K, L,
-- M, N, O, P, Q, R, S, T, U, V, W, X, Y, or Z.
upper :: Monad m => ParserT m Char
upper = toChar <$> P8.upper
