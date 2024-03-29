module MMZK.BSParser.Lexer.Regex where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe
import           MMZK.BSParser.Convert
import           MMZK.BSParser.Parser
import qualified MMZK.BSParser.Lexer as L

data Regex = Literal  ByteString -- ^ abc
           | OneOf    [Char]     -- ^ [abc]
           | NoneOf   [Char]     -- ^ [^abc]
           | Many     Regex      -- ^ a*
           | Some     Regex      -- ^ a+
           | Optional Regex      -- ^ a?
           | Choice   [Regex]    -- ^ [ab|cd|ef]
           | Sequence [Regex]    -- ^ [abc][def]
           | AnyChar             -- ^ .

-- | Parse a "ByteString" according to the given "Regex".
regexParser :: Monad m => Regex -> BSParserT e m ByteString
regexParser (Literal str)      = L.byteString str
regexParser (OneOf chs)        = toByteString . (: []) <$> L.oneOf chs
regexParser (NoneOf chs)       = toByteString . (: []) <$> L.noneOf chs
regexParser (Many regex)       = BS.concat <$> many (regexParser regex)
regexParser (Some regex)       = BS.concat <$> some (regexParser regex)
regexParser (Optional regex)   = fromMaybe BS.empty
                             <$> optional (regexParser regex)
regexParser (Choice regexes)   = msum $ regexParser <$> regexes
regexParser (Sequence regexes) = BS.concat <$> mapM regexParser regexes
regexParser AnyChar            = toByteString . (: []) <$> L.anyChar
