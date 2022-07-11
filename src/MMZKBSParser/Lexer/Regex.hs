module MMZKBSParser.Lexer.Regex where

import           Data.ByteString (ByteString)

data Regex = Literal ByteString -- ^ abc
           | OneOf   ByteString -- ^ [abc]
           | NoneOf  ByteString -- ^ [^abc]
           | Many    Regex      -- ^ a*
           | Some    Regex      -- ^ a+
           | Couldbe Regex      -- ^ a?
           | Choice  [Regex]    -- ^ [ab|cd|ef]
           | AnyChar            -- ^ .
           | EOF                -- ^ $
