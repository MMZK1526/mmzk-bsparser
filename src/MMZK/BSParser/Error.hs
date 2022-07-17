-- Error types as well as the default pretty-print function for errors.

module MMZK.BSParser.Error where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

-- | A class isomorphic to "Show" used to provide an alternative "pretty-print".
class PP a where
  pp :: a -> String

-- | A "Char" token, "String" token, or end-of-input.
data Token = CToken Char | SToken String | EOI
  deriving (Eq, Ord, Show)

instance PP Token where
  pp (CToken ch)  = '\'' : ch : "'"
  pp (SToken str) = '"' : str ++ "\""
  pp EOI          = "[end-of-input]"

-- | A "Token" with an optional label used to describe an unexpected token
-- during parsing.
data UItem = UItem (Maybe String) (Maybe Token)
  deriving (Eq, Ord, Show)

instance PP UItem where
  pp (UItem Nothing tk)    = maybe "" pp tk
  pp (UItem (Just str) tk) = maybe str (((str ++ " ") ++) . pp) tk

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
      []   -> str
      [tk] -> str ++ " " ++ pp tk
      tks' -> str ++ " (" ++ go tks' ++ ")"
    where
      tkList         = S.toAscList tks
      go []          = ""
      go [tk]        = pp tk
      go [tk, tk']   = pp tk ++ " or " ++ pp tk'
      go (tk : tks') = pp tk ++ ", " ++ go tks'

-- | The error type for "BSParser", containing the (optional) unexpected item,
-- the set of expect items, and the (possibly empty) list of custom error
-- messages.
data PError = BasicErr { unexpected  :: UItem
                       , expecting   :: Map (Maybe String) (Set Token)
                       , errMessages :: [String] }
            | BadUTF8
  deriving (Eq, Ord, Show)

instance PP PError where
  pp err@BasicErr {} = concat [go1, go2, go3]
    where
      go1 = case pp $ unexpected err of
        ""  -> ""
        str -> "Unexpected " ++ str ++ ".\n"
      go2 = case filter (not . null) . fmap (pp . uncurry EItem) 
                                     $ M.toList (expecting err) of
        []   -> ""
        strs -> "Expecting " ++ intercalate "; " strs ++ ".\n"
      go3 = case errMessages err of
        []   -> ""
        strs -> intercalate "\n" strs ++ "\n"
  pp _               = "Invalid UTF-8 codepoint.\n"

-- ｜ A "PError" together with the location information.
data ErrorSpan = ErrorSpan { esLocation :: Int -- ^ Position of the start
                           , esLength   :: Int -- ^ Length of the error
                           , esError    :: PError }
  deriving (Show)

instance Eq ErrorSpan where
  es1 == es2 = (esLocation es1, esLength es1) == (esLocation es2, esLength es2)

instance Ord ErrorSpan where
  es1 <= es2 = (esLocation es1, esLength es1) <= (esLocation es2, esLength es2)

-- | The empty "PError".
nil :: PError
nil = BasicErr (UItem Nothing Nothing) M.empty []
{-# INLINE nil #-}

-- | Set the unexpected item of the "PError" with the given label and token.
setUnexpected :: Maybe String -> Maybe Token -> PError -> PError
setUnexpected lb tk err = err { unexpected = UItem lb tk }
{-# INLINE setUnexpected #-}

-- | Append a new custom message to the "PError".
addMessage :: String -> PError -> PError
addMessage str err = err { errMessages = str : errMessages err }
{-# INLINE addMessage #-}
