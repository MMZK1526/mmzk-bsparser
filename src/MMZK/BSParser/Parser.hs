{-# LANGUAGE TupleSections #-}

module MMZK.BSParser.Parser where

import qualified Control.Applicative as A
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import           Data.Functor.Identity
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Word
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Error

data ParseState e m = ParseState
  { parseStr    :: ByteString -- ^ The input string
  , parseIndex  :: Int -- ^ The current index
  , bitOffset   :: Int -- ^ The current bit in the current word
  , allowBadCP  :: Bool -- ^ Allow invalid UTF-8 encoding (default to False)
  , spaceParser :: BSParserT e m () -- ^ How to consume space
  , errorStack  :: [ErrSpan e] -- ^ Saved errors
  , errorLocs   :: IntSet -- ^ Error locations (start & end indices)
  , tabWidth    :: Int  -- ^ Width of tab (default to 4)
  , pruneIndex  :: Int -- ^ Lower limit of backtracking
  }

instance Eq (ParseState e m) where
  a == b = parseIndex a == parseIndex b && bitOffset a == bitOffset b

instance Ord (ParseState e m) where
  a <= b = (parseIndex a < parseIndex b)
        || (parseIndex a == parseIndex b && bitOffset a <= bitOffset b)

incPS :: ParseState e m -> ParseState e m
incPS = addPS 1
{-# INLINE [2] incPS #-}

addPS :: Int -> ParseState e m -> ParseState e m
addPS n ps = ps { parseIndex = parseIndex ps + n }
{-# INLINE [2] addPS #-}

headPS :: ParseState e m -> Maybe Word8
headPS ps = BS.indexMaybe (parseStr ps) (parseIndex ps)
{-# INLINE [2] headPS #-}

firstNPS :: Int -> ParseState e m -> ByteString
firstNPS n ps = BS.take n $ BS.drop (parseIndex ps) (parseStr ps)
{-# INLINE [2] firstNPS #-}

newtype BSParserT e m a
  = BSParserT { runParserT :: ParseState e m
           -> m (Either (ErrSpan e) a, ParseState e m) }

type BSParser e a = BSParserT e Identity a

instance Monad m => Functor (BSParserT e m) where
  fmap = liftM

instance Monad m => Applicative (BSParserT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (BSParserT e m) where
  return a = BSParserT $ return . (Right a ,)
  f >>= g  = BSParserT $ \ps -> do
    (ma, ps') <- runParserT f ps
    case ma of
      Right a  -> runParserT (g a) ps'
      Left err -> pure (Left err, ps')

instance Monad m => A.Alternative (BSParserT e m) where
  empty   = BSParserT
          $ \ps -> pure (Left $ ErrSpan (parseIndex ps, parseIndex ps) nil, ps)
  f <|> g = BSParserT $ \ps -> do
    (ma, psF) <- runParserT f ps
    case ma of
      Left errF -> if pruneIndex psF >= parseIndex ps
                   && pruneIndex psF <= parseIndex psF
        then pure (Left errF, psF)
        else do
          (mb, psG) <- runParserT g ps
          case mb of
            Left errG -> pure . (Left $ errF <> errG ,) $ if errF > errG
              then psF
              else psG
            success -> pure (success, psG)
      success -> pure (success, psF)

instance Monad m => MonadPlus (BSParserT e m)

instance Monad m => MonadFail (BSParserT e m) where
  fail = const A.empty

instance MonadTrans (BSParserT e) where
  lift m = BSParserT $ \ps -> (, ps) . Right <$> m


--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

parseT :: Monad m => ByteStringLike s
       => BSParserT e m a -> s -> m (Either (ErrBundle e) a)
parseT parser str = do
  (result, ps) <- runParserT parser initState
  return $ case result of
    Right a -> Right a
    Left e  -> Left $ ErrBundle (e : errorStack ps)
                                ( IS.insert (fst $ esLoc e)
                                            (IS.insert (uncurry max (esLoc e))
                                                       (errorLocs ps)) )
                                (parseStr ps) (tabWidth ps)
  where
    initState = ParseState { parseIndex  = 0
                           , parseStr    = toByteString str
                           , allowBadCP  = False
                           , bitOffset   = 0
                           , spaceParser = pure ()
                           , errorStack  = []
                           , errorLocs   = IS.empty
                           , tabWidth    = 4
                           , pruneIndex  = -1 }
{-# INLINE [2] parseT #-}

parse :: ByteStringLike s => BSParser e a -> s -> Either (ErrBundle e) a
parse = (runIdentity .) . parseT
{-# INLINE [2] parse #-}


--------------------------------------------------------------------------------
-- ParseState Interactions
--------------------------------------------------------------------------------

-- | Get the current "ParseState".
getState :: Monad m => BSParserT e m (ParseState e m)
getState = BSParserT $ \ps -> pure (Right ps, ps)
{-# INLINE [2] getState #-}

-- | Set whether bad codepoints are allowed.
setAllowBadCP :: Monad m => Bool -> BSParserT e m ()
setAllowBadCP val = BSParserT $ \ps -> pure (Right (), ps { allowBadCP = val })
{-# INLINE [2] setAllowBadCP #-}

-- | Set the "BSParserT" used to consume extra spaces.
setSpaceParser :: Monad m => BSParserT e m a -> BSParserT e m ()
setSpaceParser p = BSParserT
                 $ \ps -> pure (Right (), ps { spaceParser = void p })
{-# INLINE [2] setSpaceParser #-}

-- | Set the "pruneIndex" to "parseIndex", stopping backtracking since the
-- current index.
prune :: Monad m => BSParserT e m ()
prune = BSParserT $ \ps -> pure (Right (), ps { pruneIndex = parseIndex ps })
{-# INLINE [2] prune #-}

-- | Set the "pruneIndex" to "parseIndex + 1", stopping backtracking
-- beyond the current index.
pruneNext :: Monad m => BSParserT e m ()
pruneNext = BSParserT
          $ \ps -> pure (Right (), ps { pruneIndex = parseIndex ps + 1 })
{-# INLINE [2] pruneNext #-}

-- | Use "prune" within the given "BSParserT". If the latter succeeds, restore
-- the "pruneIndex".
withPrune :: Monad m => BSParserT e m a -> BSParserT e m a
withPrune p = BSParserT $ \ps -> do
  (r, ps') <- runParserT p ps { pruneIndex = parseIndex ps }
  return . (r ,) $ case r of
    Left _  -> ps'
    Right _ -> ps' { pruneIndex = pruneIndex ps }
{-# INLINE [2] withPrune #-}

-- | Use "pruneNext" within the given "BSParserT". If the latter succeeds,
-- restore the "pruneIndex".
withPruneNext :: Monad m => BSParserT e m a -> BSParserT e m a
withPruneNext p = BSParserT $ \ps -> do
  (r, ps') <- runParserT p ps { pruneIndex = parseIndex ps + 1 }
  return . (r ,) $ case r of
    Left _  -> ps'
    Right _ -> ps' { pruneIndex = pruneIndex ps }
{-# INLINE [2] withPruneNext #-}

-- | Set the tab width for the parser. The tab width will affect the column
-- number of the characters in the input stream.
-- It must be at least one, otherwise the value will be ignored.
setTabWidth :: Monad m => Int -> BSParserT e m ()
setTabWidth w = BSParserT $ \ps -> pure (Right (), ps { tabWidth = w })
{-# INLINE [2] setTabWidth #-}


--------------------------------------------------------------------------------
-- Primitive Combinators
--------------------------------------------------------------------------------

-- | Get the parse result as well as the error of the "BSParserT".
inspect :: Monad m => BSParserT e m a -> BSParserT e m (Either (ErrSpan e) a)
inspect p = BSParserT $ fmap (first Right) . runParserT p
{-# INLINE [2] inspect #-}

-- | Also return the number of tokens ("Word8") consumed by the "BSParserT".
withLen :: Monad m => BSParserT e m a -> BSParserT e m (Int, a)
withLen p = BSParserT $ \ps -> do
  (r, ps') <- runParserT p ps
  pure ((parseIndex ps' - parseIndex ps ,) <$> r, ps')
{-# INLINE [2] withLen #-}

-- | Throw an "ErrSpan".
throw :: Monad m => ErrSpan e -> BSParserT e m a
throw err = BSParserT $ \ps -> pure (Left err, ps)
{-# INLINE [2] throw #-}

-- | Flush the remaining unprocessed bits in the current token ("Word8"). If the
-- input "ByteString" contains both binary and UTF-8, call this function
-- whenever switching from parsing binary to parsing UTF-8. Returns the number
-- of bits flushed.
flush :: Monad m => BSParserT e m Int
flush = BSParserT $ \ps -> pure $ case bitOffset ps of
  0 -> (Right 0, ps)
  n -> (Right (8 - n), incPS ps)
{-# INLINE [2] flush #-}

-- | Parse one token ("Word8") using the predicate function. If the predicate
-- returns "Nothing", fail the "BSParserT".
token :: Monad m => (Word8 -> Maybe a) -> BSParserT e m a
token f = BSParserT $ \ps -> return $ case headPS ps of
  Nothing -> (Left $ ErrSpan (parseIndex ps, parseIndex ps) eoiErr, ps)
  Just ch -> case f ch of
    Nothing -> (Left $ ErrSpan (parseIndex ps, parseIndex ps) (misErr ch), ps)
    Just a  -> (Right a, incPS ps)
  where
    eoiErr     = BasicErr (UItem Nothing (Just EOI)) M.empty []
    misErr ch' = BasicErr (UItem Nothing (Just . CToken $ toChar ch'))
                          M.empty []

-- | Parse the first n (n > 0) tokens ("ByteString") using the predicate
-- function. If the predicate returns "Nothing", fail the "BSParserT".
tokens :: Monad m
       => ByteStringLike s => Int -> (s -> Maybe a) -> BSParserT e m a
tokens n f = BSParserT $ \ps -> do
  let bs  = firstNPS n ps
  pure $ case f . fromByteString $ bs of
    Nothing -> ( Left $ ErrSpan ( parseIndex ps
                                , parseIndex ps + BS.length bs - 1 )
                                (misErr bs)
               , ps )
    Just as -> (Right as, addPS n ps)
  where
    toToken bs'
      | BS.length bs' == 0 = EOI
      | otherwise          = SToken bs'
    misErr bs' = BasicErr (UItem Nothing (Just $ toToken bs')) M.empty []

-- | Parse one "Char" codepoint using the predicate function. If the predicate
-- returns "Nothing", fail the "BSParserT". If the codepoint is invalid, the
-- behaviour is determined by "allowBadCP" of the "ParserState".
charToken :: Monad m => (Char -> Maybe a) -> BSParserT e m a
charToken f = BSParserT $ \ps -> do
  let ix = parseIndex ps
  pure $ case BSU.decode (BS.drop ix (parseStr ps)) of
    Nothing      -> (Left $ ErrSpan (ix, ix) eoiErr, ps)
    Just (ch, i) -> do
      let runPredicate = case f ch of
            Nothing -> (Left $ ErrSpan (ix, ix + i - 1) (misErr ch), ps)
            Just a  -> (Right a, addPS i ps)
      case ch of
        '\xFFFD' -> if firstNPS i ps /= toByteString [ch] && not (allowBadCP ps)
          then (Left $ ErrSpan (ix, ix + i - 1) BadUTF8, ps)
          else runPredicate
        _        -> runPredicate
  where
    eoiErr     = BasicErr (UItem Nothing (Just EOI)) M.empty []
    misErr ch' = BasicErr (UItem Nothing (Just $ CToken ch')) M.empty []

-- | Behave the same as "BSParserT" except it does not consume any input or
-- modify any state.
lookAhead :: Monad m => BSParserT e m a -> BSParserT e m a
lookAhead p = BSParserT $ \ps -> (, ps) . fst <$> runParserT p ps
{-# INLINE [2] lookAhead #-}


--------------------------------------------------------------------------------
-- Utility Combinators
--------------------------------------------------------------------------------

-- | A "BSParserT" that fails instantly.
empty :: Monad m => BSParserT e m a
empty = A.empty
{-# INLINE [2] empty #-}

-- | Try the first "BSParserT". If it fails, use the second one. When both
-- fails, if the errors occur at the same place, merge the errors; otherwise
-- choose the error that is further away.
(<|>) :: Monad m => BSParserT e m a -> BSParserT e m a -> BSParserT e m a
(<|>) = (A.<|>)
infixl 3 <|>
{-# INLINE [2] (<|>) #-}

-- | Attempt all "BSParserT"s in the list without consuming the input, if it
-- succeeds, use the "BSParserT" from the first argument. This operator acts as
-- a constraint.
(<&>) :: Monad m => BSParserT e m a -> [BSParserT e m b] -> BSParserT e m a
p <&> pcs = mapM_ lookAhead pcs >> p
infixl 2 <&>
{-# INLINE [2] (<&>) #-}

-- | Try all "BSParserT"s until one of them parses successfully.
choice :: Monad m => [BSParserT e m a] -> BSParserT e m a
choice = msum
{-# INLINE [2] choice #-}

-- | Parse for left paren, content, and right paren, returning only the content.
parens :: Monad m
       => BSParserT e m o -> BSParserT e m c -> BSParserT e m a
       -> BSParserT e m a
parens po pc pa = po >> pa <* pc
{-# INLINE [2] parens #-}

-- | Use the "BSParserT" zero, one, or more times.
many :: Monad m => BSParserT e m a -> BSParserT e m [a]
many = A.many
{-# INLINE [2] many #-}

-- | Similar to "many", but concatenates the result.
manyS :: Monad m => Monoid a => BSParserT e m a -> BSParserT e m a
manyS = fmap mconcat . A.many
{-# INLINE [2] manyS #-}

-- | Use the "BSParserT" one or more times.
some :: Monad m => BSParserT e m a -> BSParserT e m [a]
some = A.some
{-# INLINE [2] some #-}

-- | Similar to "some", but concatenates the result.
someS :: Monad m => Monoid a => BSParserT e m a -> BSParserT e m a
someS = fmap mconcat . A.some
{-# INLINE [2] someS #-}

-- | Parse zero or one occurrence of the content.
optional :: Monad m => BSParserT e m a -> BSParserT e m (Maybe a)
optional = A.optional
{-# INLINE [2] optional #-}

-- | Similar to "optional", but concatenates the result.
optionalS :: Monad m => Monoid a => BSParserT e m a -> BSParserT e m a
optionalS = fmap (fromMaybe mempty) . optional
{-# INLINE [2] optionalS #-}

-- | Parse the content between m (inclusive) and n (exclusive) times. If n <= m,
-- returns an error.
range :: Monad m => Int -> Int -> BSParserT e m a -> BSParserT e m [a]
range m n _
  | n <= m = empty
range m n p = go m
  where
    go i | i <= 0    = og (n - m - 1)
         | otherwise = liftM2 (:) p (go (i - 1))
    og d | d <= 0    = pure []
         | otherwise = do ma <- optional p
                          case ma of
                            Nothing -> pure []
                            Just a  -> (a :) <$> og (d - 1)

-- | Similar to "range", but concatenates the result.
rangeS :: Monad m => Monoid a
       => Int -> Int -> BSParserT e m a -> BSParserT e m a
rangeS = ((fmap mconcat .) .) . range
{-# INLINE [2] rangeS #-}

-- | Parse a list of contents separated by a separator.
sepBy :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepBy ps pa = sepBy1 ps pa A.<|> pure []
{-# INLINE [2] sepBy #-}

-- | Similar to "sepBy", but concatenates the result.
sepByS :: Monad m => Monoid a
       => BSParserT e m s -> BSParserT e m a -> BSParserT e m a
sepByS = (fmap mconcat . ) . sepBy
{-# INLINE [2] sepByS #-}

-- | Parse a list of contents separated by a separator where the latter can
-- optionally appear at the end.
sepEndBy :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepEndBy ps pa = sepBy ps pa <* optional ps
{-# INLINE [2] sepEndBy #-}

-- | Similar to "sepEndBy", but concatenates the result.
sepEndByS :: Monad m => Monoid a
          => BSParserT e m s -> BSParserT e m a -> BSParserT e m a
sepEndByS = (fmap mconcat . ) . sepEndBy
{-# INLINE [2] sepEndByS #-}

-- | Parse a non-empty list of contents separated by a separator.
sepBy1 :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepBy1 ps pa = liftM2 (:) pa (A.many (ps >> pa))
{-# INLINE [2] sepBy1 #-}

-- | Similar to "sepBy1", but concatenates the result.
sepBy1S :: Monad m => Monoid a
        => BSParserT e m s -> BSParserT e m a -> BSParserT e m a
sepBy1S = (fmap mconcat . ) . sepBy1
{-# INLINE [2] sepBy1S #-}

-- | Parse a non-empty list of contents separated by a separator where the
-- latter can optionally appear at the end.
sepEndBy1 :: Monad m => BSParserT e m s -> BSParserT e m a -> BSParserT e m [a]
sepEndBy1 ps pa = sepBy1 ps pa <* optional ps
{-# INLINE [2] sepEndBy1 #-}

-- | Similar to "sepEndBy1", but concatenates the result.
sepEndBy1S :: Monad m => Monoid a
           => BSParserT e m s -> BSParserT e m a -> BSParserT e m a
sepEndBy1S = (fmap mconcat . ) . sepEndBy1
{-# INLINE [2] sepEndBy1S #-}

-- | Use the "BSParserT" and map the result by the given function. Fails with
-- the "ByteString" been consumed if function returns Nothing.
pmap :: Monad m => (b -> Maybe a) -> BSParserT e m b -> BSParserT e m a
pmap f p = do
  ix  <- parseIndex <$> getState
  r   <- p
  ix' <- parseIndex <$> getState
  case f r of
    Just a  -> pure a
    Nothing -> do
      bs <- parseStr <$> getState
      throw $ ErrSpan { esLoc = (ix, ix' - 1)
                      , esError = withUnexpectedBS ( BS.take (ix' - ix)
                                                   $ BS.drop ix bs) nil }

-- | Use the "BSParserT" and map the result by the given monadic action. Fails
-- with the "ByteString" been consumed if the action fails.
pbind :: Monad m => (b -> BSParserT e m a) -> BSParserT e m b -> BSParserT e m a
pbind f p = do
  ix  <- parseIndex <$> getState
  r   <- p
  ix' <- parseIndex <$> getState
  r'  <- inspect $ f r
  case r' of
    Right a -> pure a
    Left _  -> do
      bs <- parseStr <$> getState
      throw $ ErrSpan { esLoc = (ix, ix' - 1)
                      , esError = withUnexpectedBS ( BS.take (ix' - ix)
                                                   $ BS.drop ix bs) nil }


--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

-- | Set the expected labels for the "BSParserT".
-- If the expected labels are already set by this operator or (<??>), the old
-- labels will be overwritten.
(<?>) :: Monad m => TextLike t => BSParserT e m a -> [t] -> BSParserT e m a
p <?> ls
  = touch id (const $ M.fromList . zip (Just . toText <$> ls) $ repeat S.empty)
          id p
{-# INLINE [2] (<?>) #-}

-- | Set all expected elements for "BSParserT".
-- If the expected labels are already set by this operator or (<?>), the old
-- labels will be overwritten.
(<??>) :: Monad m
       => BSParserT e m a -> Map (Maybe Text) (Set Token) -> BSParserT e m a
p <??> es = touch id (const es) id p
{-# INLINE [2] (<??>) #-}

-- | Modify the error thrown by the "BSParserT" by changing its unexpected
-- token, expected tokens, and the custom message.
-- The function cannot modify a "BadUTF8" error as its considered as a breaking
-- problem of the input stream. If we want to allow invalid UTF-8 encodings,
-- use @setAllowBadCP False@.
touch :: Monad m
      => (UItem -> UItem)
      -> (Map (Maybe Text) (Set Token) -> Map (Maybe Text) (Set Token))
      -> ([e] -> [e])
      -> BSParserT e m a -> BSParserT e m a
touch uf esf msgsf p = do
  result <- inspect p
  case result of
    Right a  -> pure a
    Left err -> throw $ case esError err of
      BadUTF8            -> err
      BasicErr u es msgs -> err'
        where
          err' = err { esError = BasicErr (uf u) (esf es) (msgsf msgs) }
{-# INLINE [2] touch #-}
