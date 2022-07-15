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

data ParseState m = ParseState { parseStr    :: ByteString
                               , parseIndex  :: Int
                               , bitOffset   :: Int
                               , allowBadCP  :: Bool
                               , spaceParser :: ParserT m () }

instance Eq (ParseState m) where
  a == b = parseIndex a == parseIndex b && bitOffset a == bitOffset b

instance Ord (ParseState m) where
  a <= b = (parseIndex a < parseIndex b)
        || (parseIndex a == parseIndex b && bitOffset a <= bitOffset b)

incPS :: ParseState m -> ParseState m
incPS = addPS 1
{-# INLINE incPS #-}

addPS :: Int -> ParseState m -> ParseState m
addPS n ps = ps { parseIndex = parseIndex ps + n }
{-# INLINE addPS #-}

headPS :: ParseState m -> Maybe Word8
headPS ps = BS.indexMaybe (parseStr ps) (parseIndex ps)
{-# INLINE headPS #-}

firstNPS :: Int -> ParseState m -> ByteString
firstNPS n ps = BS.take n $ BS.drop (parseIndex ps) (parseStr ps)
{-# INLINE firstNPS #-}

newtype ParserT m a
  = ParserT { runParserT :: ParseState m -> m (Maybe a, ParseState m) }

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
    { parseIndex = 0, parseStr    = toByteString str, allowBadCP = False
    , bitOffset  = 0, spaceParser = pure () }

parse :: ByteStringLike s => Parser a -> s -> Maybe a
parse = (runIdentity .) . parseT


--------------------------------------------------------------------------------
-- Primitive Combinators
--------------------------------------------------------------------------------

-- | Get the current "ParseState".
inspect :: Monad m => ParserT m (ParseState m)
inspect = ParserT $ \ps -> pure (Just ps, ps)
{-# INLINE inspect #-}

-- | Set whether bad codepoints are allowed.
setAllowBadCP :: Monad m => Bool -> ParserT m ()
setAllowBadCP val = ParserT $ \ps -> pure (Just (), ps { allowBadCP = val })
{-# INLINE setAllowBadCP #-}

-- | Set the "ParserT" used to consume extra spaces.
setSpaceParser :: Monad m => ParserT m a -> ParserT m ()
setSpaceParser p = ParserT $ \ps -> pure (Just (), ps { spaceParser = void p })
{-# INLINE setSpaceParser #-}

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
charToken f = ParserT $ \ps -> return
            $ case BSU.decode (BS.drop (parseIndex ps) (parseStr ps)) of
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
neg :: Monad m => ParserT m a -> ParserT m ()
neg p = ParserT $ \ps -> do
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

-- | Attempt all "ParserT"s in the list without consuming the input, if it
-- succeeds, use the "ParserT" from the first argument. This operator acts as
-- a constraint.
(<&>) :: Monad m => ParserT m a -> [ParserT m b] -> ParserT m a
p <&> pcs = mapM_ lookAhead pcs >> p
infixl 2 <&>

-- | Try all "ParserT"s until one of them parses successfully.
choice :: Monad m => [ParserT m a] -> ParserT m a
choice = msum

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
sepBy :: Monad m => ParserT m s -> ParserT m a -> ParserT m [a]
sepBy ps pa = sepBy1 ps pa <|> pure []

-- | Parse a list of contents separated by a separator where the latter can
-- optionally appear at the end.
sepEndBy :: Monad m => ParserT m s -> ParserT m a -> ParserT m [a]
sepEndBy ps pa = sepBy ps pa <* optional ps

-- | Parse a non-empty list of contents separated by a separator.
sepBy1 :: Monad m => ParserT m s -> ParserT m a -> ParserT m [a]
sepBy1 ps pa = liftM2 (:) pa (many (ps >> pa))

-- | Parse a non-empty list of contents separated by a separator where the
-- latter can optionally appear at the end.
sepEndBy1 :: Monad m => ParserT m s -> ParserT m a -> ParserT m [a]
sepEndBy1 ps pa = sepBy1 ps pa <* optional ps

-- | Use the "ParserT" and map the result by the given function. Fails if it
-- returns Nothing.
pmap :: Monad m => (b -> Maybe a) -> ParserT m b -> ParserT m a
pmap f p = p >>= maybe empty pure . f
{-# INLINE pmap  #-}
