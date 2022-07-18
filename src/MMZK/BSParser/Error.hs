-- Error types as well as the default pretty-print function for errors.

{-# LANGUAGE OverloadedStrings #-}

module MMZK.BSParser.Error where

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text, pack)
import qualified Data.Text as T

-- | A class similar to "Show", but returns a "Text". It is used to provide an
-- alternative "pretty-print".
class PP a where
  pp :: a -> Text

-- | A "Char" token, "String" token, or end-of-input.
data Token = CToken Char | SToken String | EOI
  deriving (Eq, Ord, Show)

instance PP Token where
  pp (CToken ch)  = T.concat ["'", T.singleton ch, "'"]
  pp (SToken str) = T.concat ["\"", pack str, "\""]
  pp EOI          = "[end-of-input]"

-- | A "Token" with an optional label used to describe an unexpected token
-- during parsing.
data UItem = UItem (Maybe String) (Maybe Token)
  deriving (Eq, Ord, Show)

instance PP UItem where
  pp (UItem Nothing tk)    = maybe "" pp tk
  pp (UItem (Just str) tk) = maybe (pack str) (((pack str <> " ") <>) . pp) tk

-- | A group of "Token" with an optional label used to describe the group of
-- tokens expected for a parsing error.
data EItem = EItem (Maybe String) (Set Token)
  deriving Show

instance Eq EItem where
  (EItem str1 _) == (EItem str2 _) = str1 == str2

instance Ord EItem where
  (EItem str1 _) <= (EItem str2 _) = str1 <= str2

instance PP EItem where
  pp (EItem mStr tks) = case mStr of
    Nothing  -> go tkList
    Just str -> case tkList of
      []   -> pack str
      [tk] -> pack str <> " " <> pp tk
      tks' -> pack str <> " (" <> go tks' <> ")"
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
                         , expecting   :: Map (Maybe String) (Set Token)
                         , errMessages :: [e] }
              | BadUTF8
  deriving (Eq, Ord, Show)

instance PP e => PP (PError e) where
  pp err@BasicErr {} = T.concat [go1, go2, go3]
    where
      go1 = case pp $ unexpected err of
        ""  -> ""
        str -> "Unexpected " <> str <> ".\n"
      go2 = case filter (not . T.null) . fmap (pp . uncurry EItem)
                                       $ M.toList (expecting err) of
        []   -> ""
        strs -> "Expecting " <> T.intercalate "; " strs <> ".\n"
      go3 = case errMessages err of
        []   -> ""
        strs -> T.intercalate "\n" (pp <$> strs) <> "\n"
  pp _               = "Invalid UTF-8 codepoint.\n"

-- ｜ A "PError" together with the location information.
data ErrSpan e = ErrSpan { esLocation :: Int -- ^ Position of the start
                         , esLength   :: Int -- ^ Length of the error
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
              -> BasicErr u
                          (M.unionWith S.union es es')
                          (msgs ++ msgs')
            (BasicErr {}, _) -> esError errF
            _                -> esError errG

-- | The error bundle that contains "ErrSpan"s as well as the original input.
data ErrBundle e = ErrBundle { ebErrors   :: [ErrSpan e]
                             , ebStr      :: ByteString
                             , ebTadWidth :: Int }
  deriving (Eq, Show) 

-- | The empty "PError".
nil :: PError e
nil = BasicErr (UItem Nothing Nothing) M.empty []
{-# INLINE nil #-}

-- | Set the unexpected item of the "PError" with the given label and token.
setUnexpected :: Maybe String -> Maybe Token -> PError e -> PError e
setUnexpected lb tk err = err { unexpected = UItem lb tk }
{-# INLINE setUnexpected #-}

-- | Append a new custom message to the "PError".
addMessage :: e -> PError e -> PError e
addMessage str err = err { errMessages = str : errMessages err }
{-# INLINE addMessage #-}
