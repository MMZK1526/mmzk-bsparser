{-# LANGUAGE TupleSections #-}

module MMZK.BSParser.Parser where

import qualified Control.Applicative as A
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Error

data ParseState e m = ParseState { parseStr    :: ByteString
                                 , parseIndex  :: Int
                                 , bitOffset   :: Int
                                 , allowBadCP  :: Bool
                                 , spaceParser :: BSParserT e m ()
                                 , parseErrors :: Set (ErrorSpan e) }

instance Eq (ParseState e m) where
  a == b = parseIndex a == parseIndex b && bitOffset a == bitOffset b

instance Ord (ParseState e m) where
  a <= b = (parseIndex a < parseIndex b)
        || (parseIndex a == parseIndex b && bitOffset a <= bitOffset b)

incPS :: ParseState e m -> ParseState e m
incPS = addPS 1
{-# INLINE incPS #-}

addPS :: Int -> ParseState e m -> ParseState e m
addPS n ps = ps { parseIndex = parseIndex ps + n }
{-# INLINE addPS #-}

headPS :: ParseState e m -> Maybe Word8
headPS ps = BS.indexMaybe (parseStr ps) (parseIndex ps)
{-# INLINE headPS #-}

firstNPS :: Int -> ParseState e m -> ByteString
firstNPS n ps = BS.take n $ BS.drop (parseIndex ps) (parseStr ps)
{-# INLINE firstNPS #-}

addErr :: Int -> Int -> PError e -> ParseState e m -> ParseState e m
addErr loc len err ps = ps { parseErrors = S.insert errSpan (parseErrors ps) }
  where
    errSpan = ErrorSpan loc len err
{-# INLINE addErr #-}

newtype BSParserT e m a
  = BSParserT { runParserT :: ParseState e m -> m (Maybe a, ParseState e m) }

type BSParser e a = BSParserT e Identity a

instance Monad m => Functor (BSParserT e m) where
  fmap = liftM

instance Monad m => Applicative (BSParserT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (BSParserT e m) where
  return a = BSParserT $ return . (Just a ,)
  f >>= g  = BSParserT $ \ps -> do
    (ma, ps') <- runParserT f ps
    case ma of
      Nothing -> pure (Nothing, ps')
      Just a  -> runParserT (g a) ps'

instance Monad m => A.Alternative (BSParserT e m) where
  empty   = BSParserT $ \ps -> pure (Nothing, addErr (parseIndex ps) 0 nil ps)
  f <|> g = BSParserT $ \ps -> do
    let psO = ps { parseErrors = S.empty }
    (ma, ps') <- runParserT f psO
    case ma of
      Nothing -> do
        let Just errF = fst <$> S.minView (parseErrors ps')
        (mb, ps'') <- runParserT g psO
        case mb of
          Nothing -> do
            let Just errG = fst <$> S.minView (parseErrors ps'')
            return . (Nothing ,) $ case compare errF errG of
              LT -> ps'' { parseErrors = S.insert errG (parseErrors ps) }
              GT -> ps'' { parseErrors = S.insert errF (parseErrors ps) }
              EQ -> let combo = ErrorSpan (esLocation errF) (esLength errF)
                              $ case (esError errF, esError errG) of
                                  (BasicErr u es msgs, BasicErr _ es' msgs')
                                    -> BasicErr u
                                                (M.unionWith S.union es es')
                                                (msgs ++ msgs')
                                  (BasicErr {}, _) -> esError errF
                                  _                -> esError errG
                    in  ps'' { parseErrors = S.insert combo (parseErrors ps) }
          success -> pure (success, ps'' { parseErrors = parseErrors ps })
      success -> pure (success, ps' { parseErrors = parseErrors ps })

instance Monad m => MonadPlus (BSParserT e m)

instance Monad m => MonadFail (BSParserT e m) where
  fail = const A.empty

instance MonadTrans (BSParserT e) where
  lift m = BSParserT $ \ps -> (, ps) . Just <$> m


--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

parseT :: Monad m => ByteStringLike s
       => BSParserT e m a -> s -> m (Either [ErrorSpan e] a)
parseT parser str = do
  (result, ps) <- runParserT parser initState
  return $ maybe (Left . S.toAscList $ parseErrors ps) Right result
  where
    initState = ParseState { parseIndex  = 0
                           , parseStr    = toByteString str
                           , allowBadCP  = False
                           , bitOffset   = 0
                           , spaceParser = pure ()
                           , parseErrors = S.empty }

parse :: ByteStringLike s => BSParser e a -> s -> Either [ErrorSpan e] a
parse = (runIdentity .) . parseT


--------------------------------------------------------------------------------
-- Primitive Combinators
--------------------------------------------------------------------------------

-- | Get the current "ParseState".
inspect :: Monad m => BSParserT e m (ParseState e m)
inspect = BSParserT $ \ps -> pure (Just ps, ps)
{-# INLINE inspect #-}

-- | Set whether bad codepoints are allowed.
setAllowBadCP :: Monad m => Bool -> BSParserT e m ()
setAllowBadCP val = BSParserT $ \ps -> pure (Just (), ps { allowBadCP = val })
{-# INLINE setAllowBadCP #-}

-- | Set the "BSParserT" used to consume extra spaces.
setSpaceParser :: Monad m => BSParserT e m a -> BSParserT e m ()
setSpaceParser p = BSParserT $ \ps -> pure (Just (), ps { spaceParser = void p })
{-# INLINE setSpaceParser #-}

-- | Flush the remaining unprocessed bits in the current token ("Word8"). If the
-- input "ByteString" contains both binary and UTF-8, call this function
-- whenever switching from parsing binary to parsing UTF-8. Returns the number
-- of bits flushed.
flush :: Monad m => BSParserT e m Int
flush = BSParserT $ \ps -> pure $ case bitOffset ps of
  0 -> (Just 0, ps)
  n -> (Just (8 - n), incPS ps)
{-# INLINE flush #-}

-- | Parse one token ("Word8") using the predicate function. If the predicate
-- returns "Nothing", fail the "BSParserT".
token :: Monad m => (Word8 -> Maybe a) -> BSParserT e m a
token f = BSParserT $ \ps -> return $ case headPS ps of
  Nothing -> (Nothing, addErr (parseIndex ps) 1 eoiErr ps)
  Just ch -> case f ch of
    Nothing -> (Nothing, addErr (parseIndex ps) 1 (mismatchErr ch) ps)
    Just a  -> (Just a, incPS ps)
  where
    eoiErr          = BasicErr (UItem Nothing (Just EOI)) M.empty []
    mismatchErr ch' = BasicErr (UItem Nothing (Just EOI))
                               (M.singleton Nothing . S.singleton . CToken
                                                    $ toChar ch') []

-- | Parse the first n tokens ("ByteString") using the predicate function. If
-- the predicate returns "Nothing", fail the "BSParserT".
tokens :: Monad m => ByteStringLike s => Int -> (s -> Maybe a) -> BSParserT e m a
tokens n f = BSParserT $ \ps -> do
  let bs  = firstNPS n ps
  let len = BS.length bs
  pure $ case f . fromByteString $ bs of
    Nothing -> (Nothing, addErr (parseIndex ps) len (mismatchErr bs) ps)
    Just as -> (Just as, addPS n ps)
  where
    mismatchErr bs' = BasicErr (UItem Nothing (Just EOI))
                               (M.singleton Nothing . S.singleton . SToken
                                                    $ fromByteString bs') []

-- | Parse one "Char" codepoint using the predicate function. If the predicate
-- returns "Nothing", fail the "BSParserT". If the codepoint is invalid, the
-- behaviour is determined by "allowBadCP" of the "ParserState".
charToken :: Monad m => (Char -> Maybe a) -> BSParserT e m a
charToken f = BSParserT $ \ps -> pure
            $ case BSU.decode (BS.drop (parseIndex ps) (parseStr ps)) of
  Nothing      -> (Nothing, addErr (parseIndex ps) 1 eoiErr ps)
  Just (ch, i) -> do
    let runPredicate = case f ch of
          Nothing -> (Nothing, addErr (parseIndex ps) i (mismatchErr ch) ps)
          Just a  -> (Just a, addPS i ps)
    case ch of
      '\xFFFD' -> if firstNPS i ps /= toByteString [ch] && not (allowBadCP ps)
        then (Nothing, addErr (parseIndex ps) i BadUTF8 ps)
        else runPredicate
      _        -> runPredicate
  where
    eoiErr          = BasicErr (UItem Nothing (Just EOI)) M.empty []
    mismatchErr ch' = BasicErr (UItem Nothing (Just EOI))
                               (M.singleton Nothing . S.singleton . CToken
                                                    $ ch') []

-- | Parse the end-of-input.
eof :: Monad m => BSParserT e m ()
eof = BSParserT $ \ps -> return . (, ps) $ case headPS ps of
  Nothing -> Just ()
  Just _  -> Nothing

-- | Behave the same as "BSParserT" except it does not consume any input or modify
-- any state.
lookAhead :: Monad m => BSParserT e m a -> BSParserT e m a
lookAhead p = BSParserT $ \ps -> (, ps) . fst <$> runParserT p ps

-- | Succeed iff the "BSParserT" fails. Will not consume any input or modify any
-- state.
neg :: Monad m => BSParserT e m a -> BSParserT e m ()
neg p = BSParserT $ \ps -> do
  (ma, _) <- runParserT p ps
  return $ case ma of
    Nothing -> (Just (), ps)
    _       -> (Nothing, ps)


--------------------------------------------------------------------------------
-- Utility Combinators
--------------------------------------------------------------------------------

-- | A "BSParserT" that fails instantly.
empty :: Monad m => BSParserT e m a
empty = A.empty
{-# INLINE empty #-}

-- | Try the first "BSParserT". If it fails, use the second one. When both fails,
-- if the errors occur at the same place, merge the errors; otherwise choose the
-- error that is further away.
(<|>) :: Monad m => BSParserT e m a -> BSParserT e m a -> BSParserT e m a
(<|>) = (A.<|>)
infixl 3 <|>
{-# INLINE (<|>) #-}

-- | Attempt all "BSParserT"s in the list without consuming the input, if it
-- succeeds, use the "BSParserT" from the first argument. This operator acts as
-- a constraint.
(<&>) :: Monad m => BSParserT e m a -> [BSParserT e m b] -> BSParserT e m a
p <&> pcs = mapM_ lookAhead pcs >> p
infixl 2 <&>
{-# INLINE (<&>) #-}

-- | Try all "BSParserT"s until one of them parses successfully.
choice :: Monad m => [BSParserT e m a] -> BSParserT e m a
choice = msum
{-# INLINE choice #-}

-- | Parse for left paren, content, and right paren, returning only the content.
parens :: Monad m => BSParserT e m o -> BSParserT e m c -> BSParserT e m a -> BSParserT e m a
parens po pc pa = po >> pa <* pc
{-# INLINE parens #-}

-- | Use the "BSParserT" zero, one, or more times.
many :: Monad m => BSParserT e m a -> BSParserT e m [a]
many = A.many
{-# INLINE many #-}

-- | Use the "BSParserT" one or more times.
some :: Monad m => BSParserT e m a -> BSParserT e m [a]
some = A.some
{-# INLINE some #-}

-- | Parse zero or one occurrence of the content.
optional :: Monad m => BSParserT e m a -> BSParserT e m (Maybe a)
optional = A.optional
{-# INLINE optional #-}

-- | Parse the content between m (inclusive) and n (exclusive) times. If n <= m,
-- returns an empty list.
range :: Monad m => Int -> Int -> BSParserT e m a -> BSParserT e m [a]
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
sepBy :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepBy ps pa = sepBy1 ps pa <|> pure []
{-# INLINE sepBy #-}

-- | Parse a list of contents separated by a separator where the latter can
-- optionally appear at the end.
sepEndBy :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepEndBy ps pa = sepBy ps pa <* optional ps
{-# INLINE sepEndBy #-}

-- | Parse a non-empty list of contents separated by a separator.
sepBy1 :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepBy1 ps pa = liftM2 (:) pa (many (ps >> pa))
{-# INLINE sepBy1 #-}

-- | Parse a non-empty list of contents separated by a separator where the
-- latter can optionally appear at the end.
sepEndBy1 :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepEndBy1 ps pa = sepBy1 ps pa <* optional ps
{-# INLINE sepEndBy1 #-}

-- | Use the "BSParserT" and map the result by the given function. Fails if it
-- returns Nothing.
pmap :: Monad m => (b -> Maybe a) -> BSParserT e m b -> BSParserT e m a
pmap f p = p >>= maybe empty pure . f
{-# INLINE pmap #-}
