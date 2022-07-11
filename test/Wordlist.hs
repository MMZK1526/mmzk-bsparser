import           MMZKBSParser
import qualified MMZKBSParser.ASCII as PA
import qualified MMZKBSParser.Lexer as L

main :: IO ()
main = print $ parseWordlist1 "Darth,Sidious,Maul,Tyranus,Vader"

sith :: [String]
sith = ["Darth", "Sidious", "Maul", "Tyranus", "Vader"]

-- | Parse a Wordlist, which is a list of words (formed by English letters)
-- separated by ','s without any extra space. For example, "foo", "apple,bear",
-- and "Darth,Sidious,Maul,Tyranus,Vader" are Wordlists, while "   spaces",
-- "I-have-dashes", "spaces, after, comma" and "trailing,comma," are not.
parseWordlist1 :: ByteStringLike s => s -> Maybe [String]
parseWordlist1 = parse (wordlistParser1 <* eof)
  where
    wordlistParser1 = sepBy (PA.char ',') (many PA.alpha)
