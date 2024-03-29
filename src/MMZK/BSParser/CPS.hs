-- Parser combinators in CPS style.

module MMZK.BSParser.CPS
  ( cons, many, some, manyS, someS, digitsStr, hexDigitsStr, octDigitsStr
  , binDigitsStr, string, optional, optionalS, range, rangeS, sepBy1
  , sepBy1S, sepBy, sepByS, sepEndBy, sepEndByS, sepEndBy1, sepEndBy1S
  , alphas, alphaDigits, identifier, parens, runCPS, choice, lexer, wrapper
  ) where

import           Control.Monad
import           Control.Applicative (Alternative, liftA2, empty, (<|>))
import           Data.Char
import           MMZK.BSParser.Convert
import qualified MMZK.BSParser.Lexer as L
import           MMZK.BSParser.Parser (BSParserT)
import qualified MMZK.BSParser.Parser as P


manyL :: Monad m => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
manyL p q = msum [liftA2 (:) p (many p q), q]

someL :: Monad m => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
someL p q = liftA2 (:) p $ msum [liftA2 (:) p (many p q), q]

consL :: Monad m  => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
consL = liftA2 (:)
{-# INLINE [2] consL #-}

optionalL :: Monad m
          => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
optionalL p q = msum [liftA2 (:) p q, q]

rangeL :: Monad m
       => Int -> Int -> BSParserT e m a -> BSParserT e m [a]
       -> BSParserT e m [a]
rangeL m n _ _
  | n <= m = empty
rangeL m n p q = go m
  where
    go i | i <= 0    = og (n - m - 1)
         | otherwise = liftA2 (:) p (go (i - 1))
    og d | d <= 0    = q
         | otherwise = do ma <- P.optional p
                          case ma of
                            Nothing -> q
                            Just a  -> (:) a <$> og (d - 1)


--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | Turns a CPS parser into a normal one by applying with @pure empty@.
runCPS :: Monoid b => Monad m
       => (BSParserT e m b -> BSParserT e m a) -> BSParserT e m a
runCPS pCPS = pCPS (pure mempty)

-- | Use the CPS parser, then consume the spaces that follows. Spaces are
-- defined by "spaceParser" of "ParseState", which is by default @pure ()@.
lexer :: Monad m
      => (BSParserT e m b -> BSParserT e m a)
      -> (BSParserT e m b -> BSParserT e m a)
lexer = (. ((P.spaceParser =<< P.getState) >>))
{-# INLINE [2] lexer #-}

-- | Set "spaceParser", use it to ignore leading spaces, run the CPS parser,
-- and use "eof" to reject extraneous inputs.
wrapper :: Monoid b => Monad m
        => BSParserT e m x -> (BSParserT e m b -> BSParserT e m a)
        -> BSParserT e m a
wrapper bp pCPS = P.setSpaceParser bp >> bp >> runCPS (pCPS . (<* L.eof))
{-# INLINE [2] wrapper #-}

-- | Parse for left paren, content, and right paren, returning only the content.
parens :: Monad m
       => BSParserT e m o -> BSParserT e m c
       -> (BSParserT e m b -> BSParserT e m a)
       -> (BSParserT e m b -> BSParserT e m a)
parens po pc pCPS = (po >>) . pCPS . (pc >>)
{-# INLINE [2] parens #-}

-- | Try all of the CPS parsers until one of them parses successfully.
choice :: Monad m
       => [BSParserT e m b -> BSParserT e m a]
       -> (BSParserT e m b -> BSParserT e m a)
choice pCPSs q = msum (($ q) <$> pCPSs)
{-# INLINE [2] choice #-}

-- | Parse with the first argument followed by the second argument.
cons :: Alternative t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
cons = liftA2 ((<|>) . pure)
{-# INLINE [2] cons #-}
{-# RULES "cons/[]" [~2] cons = consL #-}

-- | Parse with the first argument zero, one, or more times, followed by the
-- second argument.
many :: Alternative t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
many p q = msum [liftA2 ((<|>) . pure) p (many p q), q]
{-# NOINLINE [2] many #-}
{-# RULES "many/[]" [~2] many = manyL #-}

-- | Take a "ParserT" transformer and apply it zero, one, or more times.
manyS :: Monoid a => Monad m
      => (BSParserT e m a -> BSParserT e m a)
      -> (BSParserT e m a -> BSParserT e m a)
manyS pCPS q = msum [pCPS $ msum [manyS pCPS q, q], q]
{-# NOINLINE [2] manyS #-}

-- | Parse with the first argument one or more times, followed by the second
-- argument.
some :: Alternative t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
some p q = liftA2 ((<|>) . pure) p
         $ msum [liftA2 ((<|>) . pure) p (many p q), q]
{-# NOINLINE [2] some #-}
{-# RULES "some/[]" [~2] some = someL #-}

-- | Take a "ParserT" transformer and apply it one or more times.
someS :: Monoid a => Monad m
      => (BSParserT e m a -> BSParserT e m a)
      -> (BSParserT e m a -> BSParserT e m a)
someS pCPS q = pCPS $ msum [someS pCPS q, q]
{-# NOINLINE [2] someS #-}

-- | Parse with the first argument zero or one time, followed by the second
-- argument.
optional :: Alternative t => Monad m
         => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
optional p q = msum [liftA2 ((<|>) . pure) p q, q]
{-# INLINE [2] optional #-}
{-# RULES "optional/[]" [~2] optional = optionalL #-}

-- | Take a "ParserT" transformer and apply it zero or one time.
optionalS :: Alternative t => Monad m
          => (BSParserT e m (t a) -> BSParserT e m (t a))
          -> (BSParserT e m (t a) -> BSParserT e m (t a))
optionalS pCPS q = msum [pCPS q, q]
{-# INLINE [2] optionalS #-}

-- | Parse the content between m (inclusive) and n (exclusive) times, followed
-- by the last argument. If n <= m, returns an error.
range :: Alternative t => Monad m
      => Int -> Int -> BSParserT e m a -> BSParserT e m (t a)
      -> BSParserT e m (t a)
range m n _ _
  | n <= m = empty
range m n p q = go m
  where
    go i | i <= 0    = og (n - m - 1)
         | otherwise = liftA2 ((<|>) . pure) p (go (i - 1))
    og d | d <= 0    = q
         | otherwise = do ma <- P.optional p
                          case ma of
                            Nothing -> q
                            Just a  -> ((<|>) . pure) a <$> og (d - 1)
{-# NOINLINE [2] range #-}
{-# RULES "range/[]" [~2] range = rangeL #-}

-- | Take a "ParserT" transformer and apply it between m (inclusive) and n
-- (exclusive) times. If n <= m, returns an error.
rangeS :: Alternative t => Monad m
       => Int -> Int -> (BSParserT e m (t a) -> BSParserT e m (t a))
       -> (BSParserT e m (t a) -> BSParserT e m (t a))
rangeS m n _ _
  | n <= m = empty
rangeS m n pCPS q = go m
  where
    go i | i <= 0    = og (n - m - 1)
         | otherwise = pCPS . go $ i - 1
    og d | d <= 0    = q
         | otherwise = msum [pCPS . og $ d - 1, q]

-- | Parse zero or more occurrences of the second argument separated by the
-- first argument, followed by the last argument.
sepBy :: Alternative t => Monad m
       => BSParserT e m s -> BSParserT e m a -> BSParserT e m (t a)
       -> BSParserT e m (t a)
sepBy ps pa q = sepBy1 ps pa q <|> q
{-# INLINE [2] sepBy #-}

-- | Take a separator parser and "ParserT" transformer, apply the latter at
-- zero or more times.
sepByS :: Monoid a => Monad m
       => BSParserT e m s -> (BSParserT e m a -> BSParserT e m a)
       -> (BSParserT e m a -> BSParserT e m a)
sepByS ps pCPS q = sepBy1S ps pCPS q <|> q
{-# INLINE [2] sepByS #-}

-- | Parse at least one occurrence of the second argument separated by the first
-- argument, followed by the last argument.
sepBy1 :: Alternative t => Monad m
       => BSParserT e m s -> BSParserT e m a -> BSParserT e m (t a)
       -> BSParserT e m (t a)
sepBy1 ps pa q = liftM2 ((<|>) . pure) pa (many (ps >> pa) q)
{-# INLINE [2] sepBy1 #-}

-- | Take a separator parser and "ParserT" transformer, apply the latter at
-- least once.
sepBy1S :: Monoid a => Monad m
       => BSParserT e m s -> (BSParserT e m a -> BSParserT e m a)
       -> (BSParserT e m a -> BSParserT e m a)
sepBy1S ps pCPS = pCPS . manyS ((ps >>) . pCPS)
{-# INLINE [2] sepBy1S #-}

-- | Parse zero or more occurrences of the second argument separated by (and
-- optionally ended with) the first argument, followed by the last argument.
sepEndBy :: Alternative t => Monad m
       => BSParserT e m s -> BSParserT e m a -> BSParserT e m (t a)
       -> BSParserT e m (t a)
sepEndBy ps pa q = sepBy ps pa (P.optional ps >> q)
{-# INLINE [2] sepEndBy #-}

-- | Take a separator parser (can appear at the end) and "ParserT" transformer,
-- apply the latter at zero or more times.
sepEndByS :: Monoid a => Monad m
       => BSParserT e m s -> (BSParserT e m a -> BSParserT e m a)
       -> (BSParserT e m a -> BSParserT e m a)
sepEndByS ps pCPS q = sepByS ps pCPS (P.optional ps >> q)
{-# INLINE [2] sepEndByS #-}

-- | Parse at least one occurrence of the second argument separated by (and
-- optionally ended with) the first argument, followed by the last argument.
sepEndBy1 :: Alternative t => Monad m
       => BSParserT e m s -> BSParserT e m a -> BSParserT e m (t a)
       -> BSParserT e m (t a)
sepEndBy1 ps pa q = sepBy1 ps pa (P.optional ps >> q)
{-# INLINE [2] sepEndBy1 #-}

-- | Take a separator parser (can appear at the end) and "ParserT" transformer,
-- apply the latter at least once.
sepEndBy1S :: Monoid a => Monad m
       => BSParserT e m s -> (BSParserT e m a -> BSParserT e m a)
       -> (BSParserT e m a -> BSParserT e m a)
sepEndBy1S ps pCPS q = sepBy1S ps pCPS (P.optional ps >> q)
{-# INLINE [2] sepEndBy1S #-}


--------------------------------------------------------------------------------
-- String
--------------------------------------------------------------------------------

-- | Parse the given "String".
string :: Monad m => StringLike s
       => s -> BSParserT e m String -> BSParserT e m String
string str p = case toString str of
  ""       -> p
  ch : chs -> liftA2 (:) (L.char ch) (string chs p)

-- | Parse a string of (unsigned) digits.
digitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
digitsStr = some L.digitChar
{-# INLINE [2] digitsStr #-}

-- | Parse a string of (unsigned) hex digits.
hexDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
hexDigitsStr = some L.hexDigitChar
{-# INLINE [2] hexDigitsStr #-}

-- | Parse a string of (unsigned) oct digits.
octDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
octDigitsStr = some L.octDigitChar
{-# INLINE [2] octDigitsStr #-}

-- | Parse a string of (unsigned) bin digits.
binDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
binDigitsStr = some L.binDigitChar
{-# INLINE [2] binDigitsStr #-}

-- | Parse a string of alphabetic Unicode characters, following their General
-- Categories.
alphas :: Monad m => BSParserT e m String -> BSParserT e m String
alphas = some L.alpha
{-# INLINE [2] alphas #-}

-- | Parse a string of alphabetic Unicode characters or digits.
alphaDigits :: Monad m => BSParserT e m String -> BSParserT e m String
alphaDigits = some L.alphaDigit
{-# INLINE [2] alphaDigits #-}

-- | Parse a string of alphabetic Unicode characters, digits, or underscores,
-- but not starting with a digit.
identifier :: Monad m => BSParserT e m String -> BSParserT e m String
identifier = cons L.alphaDigit
           . some (L.satisfy (\ch -> isAlpha ch || isDigit ch || ch == '_'))
{-# INLINE [2] identifier #-}
