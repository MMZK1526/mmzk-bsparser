{-# LANGUAGE TupleSections #-}

module MMZKBSParser.Parser where

import qualified Control.Applicative as A
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import           Data.Functor.Identity
import           Data.Word
import           MMZKBSParser.Convert

data ParseState = ParseState { parseStr   :: ByteString
                             , parseIndex :: Int
                             , bitOffset  :: Int
                             , allowBadCP :: Bool }
  deriving Eq

instance Ord ParseState where
  a <= b = (parseIndex a < parseIndex b) || (bitOffset a <= bitOffset b)

incPS :: ParseState -> ParseState
incPS = addPS 1
{-# INLINE incPS #-}

addPS :: Int -> ParseState -> ParseState
addPS n ps = ps { parseIndex = parseIndex ps + n }
{-# INLINE addPS #-}

headPS :: ParseState -> Maybe Word8
headPS ps = BS.indexMaybe (parseStr ps) (parseIndex ps)
{-# INLINE headPS #-}

firstNPS :: Int -> ParseState -> ByteString
firstNPS n ps = BS.take n $ BS.drop (parseIndex ps) (parseStr ps)
{-# INLINE firstNPS #-}

newtype ParserT m a
  = ParserT { runParserT :: ParseState -> m (Maybe a, ParseState) }

type Parser a = ParserT Identity a

instance Monad m => Functor (ParserT m) where
  fmap = liftM

instance Monad m => Applicative (ParserT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ParserT m) where
  return a = ParserT $ return . (Just a ,)
  f >>= g  = ParserT $ \ps -> do
    (ma, ps') <- runParserT f ps
    case ma of
      Nothing -> pure (Nothing, ps')
      Just a  -> runParserT (g a) ps'

instance Monad m => A.Alternative (ParserT m) where
  empty   = ParserT $ return . (Nothing ,)
  f <|> g = ParserT $ \ps -> do
    (ma, ps') <- runParserT f ps
    case ma of
      Nothing -> runParserT g ps
      success -> pure (success, ps')

instance Monad m => MonadPlus (ParserT m)

instance Monad m => MonadFail (ParserT m) where
  fail = const A.empty

instance MonadTrans ParserT where
  lift m = ParserT $ \ps -> (, ps) . Just <$> m


--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

parseT :: Monad m => ByteStringLike s => ParserT m a -> s -> m (Maybe a)
parseT parser str = fst <$> runParserT parser ParseState
    { parseIndex = 0, parseStr = toByteString str, allowBadCP = False
    , bitOffset  = 0 }

parse :: ByteStringLike s => Parser a -> s -> Maybe a
parse = (runIdentity .) . parseT


--------------------------------------------------------------------------------
-- Primitive Combinators
--------------------------------------------------------------------------------

-- | Get the current "ParseState".
inspect :: Monad m => ParserT m ParseState
inspect = ParserT $ \ps -> pure (Just ps, ps)
{-# INLINE inspect #-}

-- | Set whether bad codepoints are allowed.
setAllowBadCP :: Monad m => Bool -> ParserT m ()
setAllowBadCP val = ParserT $ \ps -> pure (Just (), ps { allowBadCP = val })
{-# INLINE setAllowBadCP #-}

-- | Forcefully set the "ParseState". In particular, this enables jumping to a
-- specific index of the input "ByteStream" for the parser. This is a very 
-- dangerous move, for example, if the index of the new "ParseState" is out of
-- bound, the parser WILL crash. This function is not exposed in the main module
-- and must be explicitly imported from MMZKBSParser.Unsafe.
forcePush :: Monad m => ParseState -> ParserT m ()
forcePush ps = ParserT . const $ pure (Just (), ps)
{-# INLINE forcePush #-}

-- | Flush the remaining unprocessed bits in the current token ("Word8"). If the
-- input "ByteString" contains both binary and UTF-8, call this function
-- whenever switching from parsing binary to parsing UTF-8. Returns the number
-- of bits flushed.
flush :: Monad m => ParserT m Int
flush = ParserT $ \ps -> pure $ case bitOffset ps of
  0 -> (Just 0, ps)
  n -> (Just (8 - n), incPS ps)
{-# INLINE flush #-}

-- | Parse one token ("Word8") using the predicate function. If the predicate
-- returns "Nothing", fail the "ParserT".
token :: Monad m => (Word8 -> Maybe a) -> ParserT m a
token f = ParserT $ \ps -> return $ case headPS ps of
  Nothing -> (Nothing, ps)
  Just ch -> case f ch of
    Nothing -> (Nothing, ps)
    Just a  -> (Just a, incPS ps)

-- | Parse the first n tokens ("ByteString") using the predicate function. If
-- the predicate returns "Nothing", fail the "ParserT".
tokens :: Monad m => ByteStringLike s => Int -> (s -> Maybe a) -> ParserT m a
tokens n f = ParserT $ \ps -> pure $ case f . fromByteString $ firstNPS n ps of
  Nothing -> (Nothing, ps)
  Just as -> (Just as, addPS n ps)

-- | Parse one "Char" codepoint using the predicate function. If the predicate
-- returns "Nothing", fail the "ParserT". If the codepoint is invalid, the
-- behaviour is determined by "allowBadCP" of the "ParserState".
charToken :: Monad m => (Char -> Maybe a) -> ParserT m a
charToken f = ParserT $ \ps -> return $ case BSU.decode (parseStr ps) of
  Nothing      -> (Nothing, ps)
  Just (ch, i) -> case ch of
    '\xFFFD' -> if firstNPS i ps /= toByteString [ch] && not (allowBadCP ps)
      then (Nothing, ps)
      else runPredicate
    _        -> runPredicate
    where
      runPredicate = case f ch of
        Nothing -> (Nothing, ps)
        Just a  -> (Just a, addPS i ps)

-- | Parse the end-of-input.
eof :: Monad m => ParserT m ()
eof = ParserT $ \ps -> return . (, ps) $ case headPS ps of
  Nothing -> Just ()
  Just _  -> Nothing

-- | Behave the same as "ParserT" except it does not consume any input or modify
-- any state.
lookAhead :: Monad m => ParserT m a -> ParserT m a
lookAhead p = ParserT $ \ps -> (, ps) . fst <$> runParserT p ps

-- | Succeed iff the "ParserT" fails. Will not consume any input or modify any
-- state.
negate :: Monad m => ParserT m a -> ParserT m ()
negate p = ParserT $ \ps -> do
  (ma, _) <- runParserT p ps
  return $ case ma of
    Nothing -> (Just (), ps)
    _       -> (Nothing, ps)


--------------------------------------------------------------------------------
-- Utility Combinators
--------------------------------------------------------------------------------

-- | A "ParserT" that fails instantly.
empty :: Monad m => ParserT m a
empty = A.empty

-- | Try the first "ParserT". If it fails, use the second one.
(<|>) :: Monad m => ParserT m a -> ParserT m a -> ParserT m a
(<|>) = (A.<|>)
infixl 3 <|>

-- | Attempt both "ParserT"s on the same input. If both suceeds, returns a
-- tuple of the two results. The number of tokens consumed is longer one of the
-- two "ParserT"s, or the first one if they are equal. Fails if one of the
-- "ParserT" fails.
(<&&>) :: Monad m => ParserT m a -> ParserT m b -> ParserT m (a, b)
pa <&&> pb = do
  ps  <- inspect
  a   <- pa
  psA <- inspect
  forcePush ps
  b   <- pb
  psB <- inspect
  forcePush $ max psA psB
  return (a, b)
infixl 2 <&&>

-- | Attempt both "ParserT"s on the same input. If both suceeds, choose the one
-- that consumes the most tokens, or the first one if the numbers are equal.
-- Fails if one of the "ParserT" fails.
(<&>) :: Monad m => ParserT m a -> ParserT m a -> ParserT m a
p1 <&> p2 = do
  ps  <- inspect
  r1   <- p1
  ps1 <- inspect
  forcePush ps
  r2   <- p2
  ps2 <- inspect
  if ps1 >= ps2 then r1 <$ forcePush ps1 else r2 <$ forcePush ps2
infixl 2 <&>


-- | Try all "ParserT"s until one of them parses successfully.
choice :: Monad m => [ParserT m a] -> ParserT m a
choice = msum

-- | Attempt a list of "ParserT"s on the same input. If all suceeds, returns a
-- list of the results. The number of tokens consumed is longest one of the
-- "ParserT"s, or the first one of them if there is a tie. Fails if one of the 
-- "ParserT" fails.
everything :: Monad m => [ParserT m a] -> ParserT m [a]
everything parsers = do
  psO <- inspect
  let go ps []             = [] <$ forcePush ps
      go ps (p : parsers') = do
        r   <- p
        ps' <- inspect
        forcePush psO
        (r :) <$> go (max ps ps') parsers'
  go psO parsers

-- | Attempt a list of "ParserT"s on the same input. If all suceeds, choose the
-- one that has consumed the most number of tokens, or the first one of them if
-- there is a tie. Fails if the list is empty or one of the "ParserT" fails.
every :: Monad m => [ParserT m a] -> ParserT m a
every []            = empty
every (p : parsers) = do
  psO <- inspect
  r1  <- p
  ps1 <- inspect
  let go ps r []              = r <$ forcePush ps
      go ps r (p' : parsers') = do
        forcePush psO
        r'  <- p'
        ps' <- inspect
        if ps >= ps then go ps r parsers' else go ps' r' parsers'
  go ps1 r1 parsers

-- | Parse for left paren, content, and right paren, returning only the content.
parens :: Monad m => ParserT m o -> ParserT m c -> ParserT m a -> ParserT m a
parens po pc pa = po >> pa <* pc

-- | Use the "ParserT" zero, one, or more times.
many :: Monad m => ParserT m a -> ParserT m [a]
many = A.many

-- | Use the "ParserT" one or more times.
some :: Monad m => ParserT m a -> ParserT m [a]
some = A.some

-- | Parse zero or one occurrence of the content.
optional :: Monad m => ParserT m a -> ParserT m (Maybe a)
optional = A.optional

-- | Parse the content between m (inclusive) and n (exclusive) times. If n <= m,
-- returns an empty list.
range :: Monad m => Int -> Int -> ParserT m a -> ParserT m [a]
range m n _
  | n <= m = pure []
range m n p = go m
  where
    go i | i <= 0    = og (n - m - 1)
         | otherwise = liftM2 (:) p (go (i - 1))
    og d | d <= 0    = pure []
         | otherwise = do ma <- optional p
                          case ma of
                            Nothing -> pure []
                            Just a  -> (a :) <$> og (d - 1)

-- | Parse a list of contents separated by a separator.
sepBy :: Monad m => ParserT m a -> ParserT m s -> ParserT m [a]
sepBy pa ps = sepBy1 pa ps <|> pure []

-- | Parse a list of contents separated by a separator where the latter can
-- optionally appear at the end.
sepEndBy :: Monad m => ParserT m a -> ParserT m s -> ParserT m [a]
sepEndBy pa ps = sepBy pa ps <* optional ps

-- | Parse a non-empty list of contents separated by a separator.
sepBy1 :: Monad m => ParserT m a -> ParserT m s -> ParserT m [a]
sepBy1 pa ps = liftM2 (:) pa (many (ps >> pa))

-- | Parse a non-empty list of contents separated by a separator where the
-- latter can optionally appear at the end.
sepEndBy1 :: Monad m => ParserT m a -> ParserT m s -> ParserT m [a]
sepEndBy1 pa ps = sepBy1 pa ps <* optional ps
