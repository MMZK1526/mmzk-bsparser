-- Error types as well as the default pretty-print function for errors.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MMZK.BSParser.Error where

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Void
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
data ErrSpan e = ErrSpan { esLocation :: Int  -- ^ Position of the start
                         , esLength   :: Int  -- ^ Length of the error
                         , esError    :: PError e }
  deriving Show

instance Eq (ErrSpan e) where
  es1 == es2 = (esLocation es1, esLength es1) == (esLocation es2, esLength es2)

instance Ord (ErrSpan e) where
  es1 <= es2 = (esLocation es1, esLength es1) <= (esLocation es2, esLength es2)

instance Semigroup (ErrSpan e) where
  errF <> errG = case compare errF errG of
    LT -> errG
    GT -> errF
    EQ -> ErrSpan (esLocation errF) (esLength errF)
        $ case (esError errF, esError errG) of
            (BasicErr u es msgs, BasicErr _ es' msgs')
              -> BasicErr u (M.unionWith S.union es es') (msgs ++ msgs')
            (BasicErr {}, _) -> esError errF
            _                -> esError errG

-- | The error bundle that contains "ErrSpan"s as well as the original input.
data ErrBundle e = ErrBundle { ebErrors   :: [ErrSpan e]
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
renderErrBundle eb = T.concat $ showError <$> errRowCols
  where
    str           = ebStr eb
    showError erc = case pp $ fst erc of
      ""   -> T.concat [showSpan $ snd erc, "."]
      eStr -> T.concat [showSpan $ snd erc, ":\n", eStr]
    errRowCols    = go (0, (1 :: Int, 1)) (ebErrors eb)
    go entry es   = case es of
      []      -> []
      e : es' -> let entry'  = slide entry (esLocation e)
                     entry'' = slide entry' (esLocation e + esLength e - 1)
                 in  (esError e, (snd entry', snd entry'')) : go entry'' es'
    slide entry i
      | fst entry == i     = entry
      | i == BS.length str = bimap succ (second succ) (slide entry (i - 1))
      | fst entry < i      = goF entry i
      | otherwise          = goB entry i
    goF entry@(i, (r, c)) i'
      | i < i'    = case fromJust . BSU.decode $ BS.drop i str of
        ('\t', _) -> goF (i + 1, (r, c + ebTabWidth eb)) i'
        ('\n', _) -> goF (i + 1, (r, c)) i'
        ('\r', _) -> goF (i + 1, (r, c)) i'
        (_, ix)   -> if i + ix > i' then entry else goF (i + ix, (r, c + 1)) i'
      | otherwise = entry
    goB (i, (r, c)) i' -- TODO: Decode
      | i > i'    = case str `BS.index` i of
        7  -> goF (i - 1, (r, c - ebTabWidth eb)) i'
        10 -> goF (i - 1, (r - 1, c)) i'
        13 -> goF (i - 1, (r, c)) i'
        _  -> goF (i - 1, (r, c - 1)) i'
      | otherwise = (i, (r, c))
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
