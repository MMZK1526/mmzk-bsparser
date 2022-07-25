# Examples
To follow these examples, we need to [install the library](README.md#installation) first. It is also recommended to create a new ["playground" workspace](README.md#installation) as it will be assumed to exist in all example.

## Wordlist-CPS
This example is a follow-up to the [Wordlist example](README.md#quickstart), which is a simple parser that takes a "Wordlist" separated by commas and produces a list of words. For example, the input "apple, banana, cherry" would be parsed into the list `["apple", "banana", "cherry"]`. By the end of that tutorial, we managed to handle words that contain dashes and apostrophes, however, the way we did it is not the most efficient, and here we are going to explore how to combine string parsers with the CPS parsers.

To follow this example, make sure that the "playground" workspace is at the [state where we left off](examples/wordlist). The full example is also available [here](examples/wordlist_cps).

### Covered Topics
* Use CPS parsers to build efficient string parsers.

### Previous Problem
Recall our definition for `wordParser`:

```Haskell
wordParser :: Parser String
wordParser = do
  pureLetters  <- some letterParser
  symbolGroups <- manyS (prune >> groupParser)
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
wordParser = CPS.some letterParser (manyS (prune >> groupParser))
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

The definition is quite obscure and it can be best explained with an example. Suppose we want to parse words such as "buzz", "fizzbuzz", "fizzfizzbuzz", "fizzfizzfizzbuzz", *i.e.* words that start with at least one "fizz" and end with "buzz". Using the CPS version for `manyS`, we can write the parser simply as `CPS.manyS (CPS.string "fizz") (L.string "buzz")`. Note that the first argument is also a combinator that takes a string parser and produces a string parser.

With `CPS.manyS`, we can rewrite `wordParser` and eliminate string concatenations in the original `manyS`:

```Haskell
wordParser = CPS.some letterParser ((CPS.manyS (\p -> prune >> groupParser p)) (pure []))
  where
    -- Note that now groupParser is a combinator
    groupParser p = CPS.cons (choice [L.char '-', L.char '\'']) (CPS.some letterParser p)
```

Note that we put `pure []` as the last parser, since we do not wish to parse anything more after finishing with `groupParser`. We also make use of `CPS.cons`, which simply takes a char parser and a string parser, combining the result.

CPS parsers are easily composable, thus we can rewrite the definition above into point-free style if we want:

```Haskell
wordParser :: Parser String
wordParser = ( CPS.some letterParser
             . CPS.manyS ( (prune >>)
                         . CPS.cons (choice [L.char '-', L.char '\''])
                         . CPS.some letterParser ) ) (pure [])
```

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
