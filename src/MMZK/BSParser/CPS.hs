-- Parser combinators in CPS style.

module MMZK.BSParser.CPS
  ( cons, many, some, manyS, someS, digitsStr, hexDigitsStr, octDigitsStr
  , binDigitsStr ) where

import           Control.Monad
import           Control.Applicative (Alternative, liftA2, (<|>))
import qualified MMZK.BSParser.Lexer as L
import           MMZK.BSParser.Parser (BSParserT)
import qualified MMZK.BSParser.Parser as P


--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | Parse with the first argument followed by the second argument.
cons :: Alternative t => Foldable t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
cons = liftA2 ((<|>) . pure)
{-# INLINE [2] cons #-}
{-# RULES "cons/[]" [~2] cons = consL #-}

-- | Parse with the first argument zero, one, or more times, followed by the
-- second argument.
many :: Alternative t => Foldable t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
many p q = msum [liftA2 ((<|>) . pure) p (P.prune >> many p q), q]
{-# NOINLINE [2] many #-}
{-# RULES "many/[]" [~2] many = manyL #-}

-- | Parse with the first argument one or more times, followed by the second
-- argument.
some :: Alternative t => Foldable t => Monad m
     => BSParserT e m a -> BSParserT e m (t a) -> BSParserT e m (t a)
some p q = liftA2 ((<|>) . pure) p 
         $ msum [liftA2 ((<|>) . pure) p (P.prune >> many p q), q]
{-# NOINLINE [2] some #-}
{-# RULES "some/[]" [~2] some = someL #-}

-- | Take a "ParserT" transformer and apply it zero, one, or more times.
manyS :: Alternative t => Foldable t => Monad m
      => (BSParserT e m (t a) -> BSParserT e m (t a))
      -> (BSParserT e m (t a) -> BSParserT e m (t a))
manyS pCPS q = msum [pCPS $ msum [manyS pCPS q, q], q]
{-# NOINLINE [2] manyS #-}

-- | Take a "ParserT" transformer and apply it one, or more times.
someS :: Alternative t => Foldable t => Monad m
      => (BSParserT e m (t a) -> BSParserT e m (t a))
      -> (BSParserT e m (t a) -> BSParserT e m (t a))
someS pCPS q = pCPS $ msum [manyS pCPS q, q]
{-# NOINLINE [2] someS #-}

consL :: Monad m  => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
consL = liftA2 (:)
{-# INLINE [2] consL #-}

manyL :: Monad m => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
manyL p q = msum [liftA2 (:) p (P.prune >> many p q), q]

someL :: Monad m => BSParserT e m a -> BSParserT e m [a] -> BSParserT e m [a]
someL p q = liftA2 (:) p $ msum [liftA2 (:) p (P.prune >> many p q), q]


--------------------------------------------------------------------------------
-- String
--------------------------------------------------------------------------------

-- | Parse a string of (unsigned) digits.
digitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
digitsStr = some L.digit
{-# INLINE [2] digitsStr #-}

-- | Parse a string of (unsigned) hex digits.
hexDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
hexDigitsStr = some L.hexDigit
{-# INLINE [2] hexDigitsStr #-}

-- | Parse a string of (unsigned) oct digits.
octDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
octDigitsStr = some L.octDigit
{-# INLINE [2] octDigitsStr #-}

-- | Parse a string of (unsigned) bin digits.
binDigitsStr :: Monad m => BSParserT e m String -> BSParserT e m String
binDigitsStr = some L.binDigit
{-# INLINE [2] binDigitsStr #-}
