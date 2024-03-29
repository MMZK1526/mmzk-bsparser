# Examples
To follow these examples, we need to [install the library](README.md#installation) first. It is also recommended to create a new ["playground" workspace](README.md#installation) as it will be assumed to exist in all example.

## Contents
1. [Kuohu](#kuohu)
2. [JSON](#json)
3. [Wordlist-CPS](#wordlist-cps)
4. [Kuohu-CPS](#kuohu-cps)

## Kuohu
In this example, we will parse a simple structure of "well-defined" parantheses and square brackets such that all pairs match. For instance, "([])()" is well-defined because each left parenthesis matches with a right parenthesis, and vice versa. On the other hand, "(]" is not well-defined since "]" does not match with "(".

The full example is available [here](examples/kuohu).

### Covered Topics
* Use basic lexers to parse a single given character
* Simple recursive descent
* Use the combinator `parens` to build parsers that parse a pattern enclosed between a pair of brackets

### Preparation
As usual, in "app/Main.hs", replace the existing code with the following:

```Haskell
import           Data.Char
import           MMZK.BSParser
import qualified MMZK.BSParser.Lexer as L

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

test :: Show a => Parser a -> String -> IO ()
test parser inputStr = putStrLn $ case parse (parser <* L.eof) inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err
```

We then define the `Kuohu`[^1] data structure as following:

```Haskell
data Kuohu = Empty
           | Apply [Kuohu]
           | Paren Kuohu
           | Brack Kuohu
  deriving Show
```

For example, "([])()" would be parsed into `Apply [Paren (Brack Empty), Paren Empty]`. Note that we allow the empty string "".

### Recursive Descent
Recursive descent is an important concept for parser combinators. It is a top-down approach which structure closely resembles the grammar. For example, we can write the grammar of our `Kuohu` as the following:

```EBNF
Kuohu  := {Single}
Single := '(' Kuohu ')' | '[' Kuohu ']'
```

Here, `Kuohu` represents the entire string of brackets while `Single` represents one nested component. To parse a `S` (for instance "([])()"), assuming that we already know how to parse `Single`, we then simply need to apply the `many` combinator with the `Single` parser. Thus we can write something like:

```Haskell
-- The "Kuohu" parser.
kuohuParser :: Parser Kuohu
kuohuParser = do
  kuohus <- many singleParser -- "singleParser" parses a single nested component
  return $ case kuohus of
    []      -> Empty
    [kuohu] -> kuohu
    _       -> Apply kuohus

-- | The "Single" parser.
singleParser :: Parser Kuohu
singleParser = do
  undefined -- Not implemented yet
```

To parse a `Single` (for instance "([])"), we need to first parse a left bracket, then use the parser for the entire structure (`Kuohu`), and finally parse a matching right bracket. Note that here we are "recursively using" `kuohuParser` within `singleParser`. There are two types of opening brackets, namely '(' and '[', and we will start by only considering the former:

```Haskell
-- | The "Single" parser.
singleParser :: Parser Kuohu
singleParser = do
  innerKuohu <- parens (L.char '(') (L.char ')') kuohuParser
  return (Paren innerKuohu)
```

The combinator `parens` takes a parser for the left bracket, a parser for the right bracket, then a parser for the content between them. It basically parses "(...)" where "..." itself can be parsed into a `Kuohu`. As we can see, here we have a recursive pattern between `kuohuParser` and `singleParser` that looks identical to our grammer, which also has recursion.

Now we can try to test for strings fully comprised of '('s and ')'s. To test our parser, run `cabal repl` from the root directory of "playground":

```txt
> test kuohuParser "()"
Paren Empty
> test kuohuParser "()(())()"
Apply [Paren Empty,Paren (Paren Empty),Paren Empty]
> test kuohuParser "(]"
Syntax error at row 1 col 2:
  Unexpected ']'.
  Expecting ')'.
```

Lastly, to also parse for square brackets, use the combinator `choice`:

```Haskell
-- | The "Single" parser.
singleParser :: Parser Kuohu
singleParser = choice [ do innerKuohu <- parens (L.char '(') (L.char ')') kuohuParser
                           return (Paren innerKuohu)
                      , do innerKuohu <- parens (L.char '[') (L.char ']') kuohuParser
                           return (Brack innerKuohu) ]
```

Of course, we can simplify the inner `do`-notations with functor operation. We also use `pruneNext` to avoid backtracking, since if the parser fails in after successfully parsing a '(', there is no point to try '['.

```Haskell
-- | The "Single" parser.
singleParser :: Parser Kuohu
singleParser = pruneNext
          >> choice [ Paren <$> parens (L.char '(') (L.char ')') kuohuParser
                    , Brack <$> parens (L.char '[') (L.char ']') kuohuParser ]
```

Now we can handle mixed brackets:

```txt
> test kuohuParser "([])[()]"
Apply [Paren (Brack Empty),Brack (Paren Empty)]
```

### Conclusion
By this point, we have completed this simple parser for nested brackets. It is also easy to add more types of brackets, such as braces and chevrons, as demonstrated in [one of the test files](test/Kuohu.hs).

However, there remains one more niche regarding the error message. Taking `(]` as an example, clearly it is an invalid `Kuohu` since ']' does not match with '(', which is what the error message would describe. However, it is also valid to enter another left bracket as long as it is matched later on, but the error message does not include them since it does not know what will follow. If we want the error message to also expect for '(' and '[', we can use the CPS version, which tutorial can be found [here](#kuohu-cps).

## JSON
This JSON parser follows the [ECMA-404 JSON Data Interchange Standard](https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf).

In JSON, there are six types of values, namely object, array, string, number, boolean, and null. Below is the data type that we will parse the JSON string into. For simplicity, we combine boolean (true and false) with null into a single constructor.

```Haskell
data JSON = Obj [(String, JSON)] -- ^ object
          | Arr [JSON]           -- ^ array
          | Str String           -- ^ string
          | Num Double           -- ^ number (the JSON standard does not specify the precision; here we use Double)
          | Lit Lit              -- ^ boolean or null
  deriving (Eq, Ord)

data Lit = JTrue | JFalse | JNull
  deriving (Eq, Ord)
```

Among the parsers for these constructors, the simplest one is natually the parser for `Lit`. A simple solution would be to use `choice` for the three cases:
```Haskell
-- | Parse "true", "false", and "null".
jLit :: Monad m => BSParserT e m Lit
jLit = choice [JTrue <$ L.string "true", JFalse <$ L.string "false", JNull <$ L.string "null"]
```

This parser has the correct behaviour, may produce less informative error messages. For example, if the input is "true666", it will throw an error at the first '6' instead of treating the entire string as a whole. Therefore, an improvement would be to parse for a string of identifier (letters or digits, but not starting with a digit), then check if it is one of "true", "false", or "null".

```Haskell
-- | Parse "true", "false", and "null".
jLit :: Monad m => BSParserT e m Lit
jLit = pmap (`lookup` [("true", JTrue), ("false", JFalse), ("null", JNull)])
            L.identifier <?> ["JSON literal"]
```

Here we use the combinator `pmap`, which takes a parser (here it is `alphas` that parses at least one letter) and a partial function from the result parsed by the parser to the `Lit` data type. If the result from `alphas` is "true", "false", or "null", it is translated into the corresponding `Lit` value. Otherwise, the `lookup` results in `Nothing`, which fails the parser.

## Wordlist-CPS
This example is a follow-up to the [Wordlist example](README.md#quickstart), which is a simple parser that takes a "Wordlist" separated by commas and produces a list of words. For example, the input "apple, banana, cherry" would be parsed into the list `["apple", "banana", "cherry"]`. By the end of that tutorial, we managed to handle words that contain dashes and apostrophes, however, the way we did it is not the most efficient, and here we are going to explore how to combine string parsers with the CPS parsers.

To follow this example, make sure that the "playground" workspace is at the [state where we left off](examples/wordlist). The full example is also available [here](examples/wordlist_cps).

### Covered Topics
* Use non-recursive CPS combinators to build efficient string parsers

### Previous Problem
Recall our definition for `wordParser`:

```Haskell
wordParser :: Parser String
wordParser = do
  pureLetters  <- some letterParser
  symbolGroups <- manyS (pruneNext >> groupParser)
  return $ pureLetters ++ symbolGroups
  where
    groupParser = do
      symbol  <- choice [L.char '-', L.char '\'']
      letters <- some letterParser
      return $ symbol : letters
```

Here we used the list concatenation operator `(++)`, which can be quite inefficient if the first operand (namely `pureLetters`) is very long. For example, if the word to be parsed contains a thousand letters followed by a "'s", then each of the first thousand letters will be accessed twice, once during `some letterParser` and once by `(++)`. However, we cannot avoid it while still using `some` to parse the group of letters before the first punctuation.

### CPS Parsers
The introduction of "CPS (Continuation Passing Style) parsers" solve the problem above. To use them, add the following line to the import statements:

```Haskell
import qualified MMZK.BSParser.CPS as CPS
```

Let's look at the (much simplified) signature of our first CPS parser:

```Haskell
some :: BSParser e a -> BSParser e [a] -> BSParser e [a]
```

In this style, the combinator `CPS.some` now takes an extra parser which describes what to do after using the first parser one or more times. For example, `CPS.some letterParser (L.string "123")` means parsing one or more letters followed by the string literal "123". In other word, it is equivalent to

```Haskell
do letters     <- some letterParser -- Note this is the old "some"
   oneTwoThree <- L.string "123"
   return $ letters ++ oneTwoThree
```

The CPS parsers are more efficient since its implementation only process the string exactly once, while the explicit usage of `(++)` re-traverses parts of the string.

In this way, we can refactor `wordParser` as following:

```Haskell
wordParser :: Parser String
wordParser = CPS.some letterParser (manyS (pruneNext >> groupParser))
  where
    groupParser = do
      symbol  <- choice [L.char '-', L.char '\'']
      letters <- some letterParser
      return $ symbol : letters
```

There are more CPS parsers, such as `CPS.string`, which parse the given string followed by the given follow-up parser. For example, if we want to parse a word that starts with "fizz", we may write `CPS.string "fizz" (many L.alpha)`. Other CPS parsers are for parsing strings with specific properties, such as digits (TODO: Documentation).

To test our parser, run `cabal repl` from the root directory of "playground", as we did before in the original ["Wordlist" tutorial](README.md#quickstart):

```txt
> test wordlistParser "Satine, Bo-Katan"
["Satine","Bo-Katan"]
> test wordlistParser "'Front"
Syntax error at row 1 col 1:
  Unexpected '\''.
  Expecting ascii letter.
> test wordlistParser "double--dash"
Syntax error at row 1 col 8:
  Unexpected '-'.
  Expecting ascii letter.
```

### Higher-Order CPS Parsers
In fact, we can even do better. There is also a CPS variation for `manyS`, which takes a **combinator** for string parsers, and produces a combinator by self-applying the argument for zero, one, or more times:

```Haskell
-- Again, the signature is simplified.
manyS :: (Parser [a] -> Parser [a]) -> (Parser [a] -> Parser [a])
```

The definition is quite obscure and it can be best explained with an example. Suppose we want to parse words such as "buzz", "fizzbuzz", "fizzfizzbuzz", "fizzfizzfizzbuzz", *i.e.* words that start with at zero or more "fizz"s and end with one "buzz". Using the CPS version for `manyS`, we can write the parser simply as `CPS.manyS (CPS.string "fizz") (L.string "buzz")`. Note that the first argument is also a combinator that takes a string parser and produces a string parser.

With `CPS.manyS`, we can rewrite `wordParser` and eliminate string concatenations in the original `manyS`:

```Haskell
wordParser = CPS.some letterParser ((CPS.manyS (\p -> pruneNext >> groupParser p)) (pure []))
  where
    -- Note that now groupParser is also a combinator
    groupParser p = CPS.cons (choice [L.char '-', L.char '\'']) (CPS.some letterParser p)
```

Note that we put `pure []` as the last parser, since we do not wish to parse anything more after finishing with `groupParser`. We also make use of `CPS.cons`, which simply takes a char parser and a string parser, combining the result.

CPS parsers are easily composable, thus we can rewrite the definition above into point-free style if we want:

```Haskell
wordParser :: Parser String
wordParser = CPS.runCPS 
           $ CPS.some letterParser
           . CPS.manyS ( (pruneNext >>)
                       . CPS.cons (choice [L.char '-', L.char '\''])
                       . CPS.some letterParser )
```

Here, `runCPS` simply means apply `pure []` to the CPS parser.

Finally, we verify that the parser works as expected:

```txt
> test wordlistParser "Satine, Bo-Katan"
["Satine","Bo-Katan"]
> test wordlistParser "'Front"
Syntax error at row 1 col 1:
  Unexpected '\''.
  Expecting ascii letter.
> test wordlistParser "double--dash"
Syntax error at row 1 col 8:
  Unexpected '-'.
  Expecting ascii letter.
```

## Kuohu-CPS
TODO 

[^1]: "Kuohu" (括弧) means all kinds of brackets, including round ones, square ones, braces, chevrons, and so on. In our example, however, we only deal with round and square brackets for simplicity.
