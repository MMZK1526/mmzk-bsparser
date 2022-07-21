-- Error types as well as the default pretty-print function for errors.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MMZK.BSParser.Error where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.IntMap as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Void
import           MMZK.BSParser.Debug
import           MMZK.BSParser.Convert

-- | A class similar to "Show", but returns a "Text". It is used to provide an
-- alternative "pretty-print".
class PP a where
  pp :: a -> Text

instance PP String where
  pp = pack

instance PP Text where
  pp = id

instance PP Void where
  pp = absurd

-- | A "Char" token, "ByteString" token, or end-of-input.
data Token = CToken Char | SToken ByteString | EOI
  deriving (Eq, Ord, Show)

instance PP Token where
  pp (CToken ch)  = T.concat ["'", T.singleton ch, "'"]
  pp (SToken str) = T.concat ["\"", fromByteString str, "\""]
  pp EOI          = bilEOF defaultBuiltInLabels

-- | A "Token" with an optional label used to describe an unexpected token
-- during parsing.
data UItem = UItem (Maybe Text) (Maybe Token)
  deriving (Eq, Ord, Show)

instance PP UItem where
  pp (UItem Nothing tk)    = maybe "" pp tk
  pp (UItem (Just txt) tk) = maybe txt (((txt <> " ") <>) . pp) tk

-- | A group of "Token" with an optional label used to describe the group of
-- tokens expected for a parsing error.
data EItem = EItem (Maybe Text) (Set Token)
  deriving Show

instance Eq EItem where
  (EItem str1 _) == (EItem str2 _) = str1 == str2

instance Ord EItem where
  (EItem str1 _) <= (EItem str2 _) = str1 <= str2

instance PP EItem where
  pp (EItem mStr tks) = case mStr of
    Nothing  -> go tkList
    Just txt -> case tkList of
      []   -> txt
      [tk] -> txt <> " " <> pp tk
      tks' -> txt <> " (" <> go tks' <> ")"
    where
      tkList         = S.toAscList tks
      go []          = ""
      go [tk]        = pp tk
      go [tk, tk']   = pp tk <> " or " <> pp tk'
      go (tk : tks') = pp tk <> ", " <> go tks'

-- | The error type for "BSParser", containing the (optional) unexpected item,
-- the set of expect items, and the (possibly empty) list of custom error
-- messages.
data PError e = BasicErr { unexpected  :: UItem
                         , expecting   :: Map (Maybe Text) (Set Token)
                         , errMessages :: [e] }
              | BadUTF8
  deriving (Eq, Ord, Show)

instance PP e => PP (PError e) where
  pp err@BasicErr {} = T.concat [go1, go2, go3]
    where
      go []          = ""
      go [tk]        = tk
      go [tk, tk']   = tk <> " or " <> tk'
      go (tk : tks') = tk <> ", " <> go tks'
      go1 = case pp $ unexpected err of
        ""  -> ""
        str -> "  Unexpected " <> str <> "."
      go2 = case filter (not . T.null) . fmap (pp . uncurry EItem)
                                       $ M.toList (expecting err) of
        []   -> ""
        txts -> "\n  Expecting " <> go txts <> "."
      go3 = case errMessages err of
        []   -> ""
        txts -> "\n  " <> T.intercalate "  \n" (pp <$> txts)
  pp _               = "  Invalid UTF-8 codepoint."

-- ｜ A "PError" together with the location information.
data ErrSpan e = ErrSpan { esLoc   :: (Int, Int) -- ^ Start and end indices
                         , esError :: PError e }
  deriving Show

instance Eq (ErrSpan e) where
  es1 == es2 = esLoc es1 == esLoc es2

instance Ord (ErrSpan e) where
  es1 <= es2 = esLoc es1 <= esLoc es2

instance Semigroup (ErrSpan e) where
  errF <> errG = case compare errF errG of
    LT -> errG
    GT -> errF
    EQ -> ErrSpan (esLoc errF)
        $ case (esError errF, esError errG) of
            (BasicErr u es msgs, BasicErr _ es' msgs')
              -> BasicErr u (M.unionWith S.union es es') (msgs ++ msgs')
            (BasicErr {}, _) -> esError errF
            _                -> esError errG

-- | The error bundle that contains "ErrSpan"s as well as the original input.
data ErrBundle e = ErrBundle { ebErrors   :: [ErrSpan e]
                             , ebLocs     :: IntSet
                             , ebStr      :: ByteString
                             , ebTabWidth :: Int }
  deriving (Eq, Show)

-- | Built-in error labels.
data BuiltInLabels = BuiltInLabels { bilSpace    :: Text
                                   , bilDigit    :: Text
                                   , bilHexDigit :: Text
                                   , bilOctDigit :: Text
                                   , bilNum      :: Text
                                   , bilAlpha    :: Text
                                   , bilUpper    :: Text
                                   , bilLower    :: Text
                                   , bilAscii    :: Text
                                   , bilEOF      :: Text }

-- | Default error labels.
defaultBuiltInLabels :: BuiltInLabels
defaultBuiltInLabels = BuiltInLabels { bilSpace    = "space"
                                     , bilDigit    = "digit"
                                     , bilHexDigit = "hex digit"
                                     , bilOctDigit = "oct digit"
                                     , bilNum      = "number char"
                                     , bilAlpha    = "letter"
                                     , bilUpper    = "uppercase letter"
                                     , bilLower    = "lowercase letter"
                                     , bilAscii    = "ascii char"
                                     , bilEOF      = "<end of input>" }

-- | Pretty-print the errors in the "ErrBundle" as a "Text".
renderErrBundle :: PP e => ErrBundle e -> Text
renderErrBundle eb = T.concat $ showError <$> ebErrors eb
  where
    str           = ebStr eb
    showError err = case ( IM.lookup (fst $ esLoc err) errRowCols
                         , IM.lookup (snd $ esLoc err) errRowCols ) of 
      (Just s, Just e) -> case pp $ esError err of
        ""   -> T.concat [showSpan (s, e), "."]
        eStr -> T.concat [showSpan (s, e), ":\n", eStr]
      _                -> __UNREACHABLE__
                        $ let [(_, s)] = work (0, (1, 1)) [fst $ esLoc err]
                              [(_, e)] = work (0, (1, 1)) [snd $ esLoc err]
                          in case pp $ esError err of
        ""   -> T.concat [showSpan (s :: (Int, Int), e), "."]
        eStr -> T.concat [showSpan (s, e), ":\n", eStr]
    errRowCols    = IM.fromList $ work (0, (1 :: Int, 1))
                                       (IS.toAscList $ ebLocs eb)
    work _ []     = []
    work entry@(i, (r, c)) ixs@(i' : ixs')
      | i < i'    = case fromJust . BSU.decode $ BS.drop i str of
        ('\t', _) -> work (i + 1, (r, c + ebTabWidth eb)) ixs
        ('\n', _) -> work (i + 1, (r + 1, c)) ixs
        ('\r', _) -> work (i + 1, (r, c)) ixs
        (_, ix)   -> if i + ix > i'
          then (i', (r, c)) : work entry ixs'
          else work (i + ix, (r, c + 1)) ixs
      | i == i'   = entry : work entry ixs'
      | otherwise = __UNREACHABLE__ $ work (0, (1, 1)) ixs
    showSpan ((r, c), (r', c'))
      | r == r' && c == c' = T.concat [ "Syntax error at row ", pack $ show r
                                      , " col ", pack $ show c]
      | r == r'            = T.concat [ "Syntax error at row ", pack $ show r
                                      , " col ", pack $ show c, " - "
                                      , pack $ show c']
      | otherwise          = T.concat [ "Syntax error between row "
                                      , pack $ show r , " col ", pack $ show c
                                      , " and row ", pack $ show r', " col "
                                      , pack $ show c']

-- | Pretty-print the errors in the "ErrBundle" as a "String".
renderErrBundleAsStr :: PP e => ErrBundle e -> String
renderErrBundleAsStr = unpack . renderErrBundle
{-# INLINE renderErrBundleAsStr #-}

-- | The empty "PError".
nil :: PError e
nil = BasicErr (UItem Nothing Nothing) M.empty []
{-# INLINE nil #-}
