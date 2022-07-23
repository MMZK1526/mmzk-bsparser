# mmzk-bsparser
`mmzk-bsparser` is a Monadic parser combinator library aiming for paring plain texts (encoded in UTF-8 or Latin-1), binary stream, or a mixture of both. It uses `ByteString` as the underlining input stream, thus enabling fast random-access/backtracking as well as easy switch between plain texts and binary streams. By default, it can also parse `String`, strict `Text`, and `[Word8]` by converting them to and from `ByteString`.

The library is currently in its first stage of development, thus most of the functionality and documentation are not available yet.

For a quick example on how to install and use this library, check [here](#quickstart).

The library is one of my side-projects and is inspired by many other in functional programming languages, such as [megaparsec](https://hackage.haskell.org/package/megaparsecs) and [parsley](https://index.scala-lang.org/http4s/parsley). In particular, the author of `parsley` has given my teammates and me valuable advices regarding the design and usage of parser combinators during our compiler coursework. In parallel to its development, I am also planning to use it to make parsers for a number of different grammars, including JSON and BSON.

## Contents
TODO

## Installation
Since this library is not yet published, we need to install it manually before using it. Before installation, we assume that `GHC` and `cabal` are available ([here](https://www.haskell.org/cabal/) on how to install them).

In this section, we will install the library to a new `cabal` workspace.

1. Clone this repo.
2. Create a new `cabal` workspace named "playground" by doing the following in terminal:
    ```bash
    mkdir playground
    cd playground
    cabal init
    ```
    This will generate a `cabal` workspace with the source code in the "app/" folder.
3. At the root directory of "playground", create a new file called "cabal.project". Fill it with the following content:
    ```
    packages: ./
              <path-to-this-repo>
    ```
4. Now we can import the library. For example, in "app/Main.hs", we can write:
    ```Haskell
    main :: IO ()
    main = case parse L.eof "" of
        Right a -> print a
        Left _  -> putStrLn "has error"
    ```
5. Run `cabal run`, the library and the `Main` module would be built and the result should be `()`, showing that the `eof` parser (which is equivalent to parsing an empty string when used alone) is successful. If we change the string to a non-empty one and run `cabal run` again, it should print a `"has error"`.

In the [quickstart](#quickstart) below and each of the [examples](#Examples), we will assume an empty playground as above and work from there.

## Quickstart
In this example, we will make a very simple parser that takes a "Wordlist" separated by commas and produces a list of words. For example, the input "apple, banana, cherry" would be parsed into the list `["apple", "banana", "cherry"]`. This is a very simple functionality that can be easily implemented without any kind of parser combinators, but it is nevertheless an easy example to demonstrate how those combinators work. We will introduce more complicated examples in the [next section](#examples).  

To follow this example, make sure that the ["playground" workspace](#installation) is ready. The full example is also available [here](examples/wordlist).   

### First Attempt
In "app/Main.hs", replace the existing code with the following:

```Haskell
import           Data.Char
import           MMZK.BSParser
import qualified MMZK.BSParser.Lexer as L

-- | A basic parser type that uses "String" as the custom error type.
type Parser a = BSParser String a

test :: Show a => Parser a -> String -> IO ()
test parser inputStr = putStrLn $ case parse parser inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err

letterParser :: Parser Char
letterParser = L.alpha
```

Firstly, we imports the main module (`MMZK.BSParser`) which contains the core functionalities, and the lexer module (`MMZK.Lexer`) which contains useful utility lexers that we are going to use shortly.

Next we defines a type alias `Parser` based on `BSParser`, which takes two parameters, namely the custom error type and the return type. In this basic tutorial, we will not touch custom error types, thus we set it to `String`, which is already implemented for us. The most general form of the parser is `BSParserT`, which is a monad transformer that enables adding other features into our parser such as I/O, but we shall not discuss it now. Usually, `type Parser a = BSParser String a` is already sufficient for our requirements.

Then we creates the function `test`, which takes a parser and the input string, runs the parser, then prints out the result or the error message. It uses two functions from the library. The function `parse` takes a parser and an input (which could be a `String`, `Text` or `ByteString`, but here we keep with `String` for simplicity), returning either the result or an `ErrorBundle String` which represents the parsing errors. We can then use `renderErrBundleAsStr` to pretty-print the bundle.

Lastly, we defines a very simple `letterParser`, which preduces a letter. We simply uses the `alpha` function from the lexer module which consumes exactly one letter and errs if the consumed character is not a letter.

To demonstrate it, run `cabal repl` from the root directory of "playground" and type in `test letterParser "a"` in `GHCi`, and the result should be simple 'a'. Conversely, if we input something that is not a letter, the parser will complain (`>` indicates user input):

```shell
> test letterParser "1"
Syntax error at row 1 col 1:
  Unexpected '1'.
  Expecting letter.
> test letterParser "mmzk1526"
'm'
```

Careful readers may notice that the input `"mmzk1526"` passes the parser despite it contains digits. This is because our letter parser only cares about the first character it encounters (namely 'm') and discards the rest of the input. Usually, we want the parser to match ALL of the input, and if there are extraneous texts, the parser should fail. To do so, we modify the first line in the definition of `test` to `test parser inputStr = putStrLn $ case parse (parser <* L.eof) inputStr of`. Here `parser <* L.eof` means run the parser, check if the input has ended, then return the result of the parser. Now if we try the same example, the parser should complain at the second 'm':

```shell
> test letterParser "mmzk1526"
Syntax error at row 1 col 2:
  Unexpected 'm'.
  Expecting <end of input>.
'm'
```

The layout of the error message is fully configurable, which is useful if we want it to be in a different language. We will introduce how to do that in later examples.

Now, let's build a word parser by adding the following to the end of the file:

```Haskell
wordParser :: Parser String
wordParser = some letterParser
```

The function `some` is a parser combinator that takes the argument (which is another parser) and run it **at least once**, returning the list of results. Since a word consists of at least one consecutive letter, `wordParser` parses exactly one word (we don't care if the word is meaningful, *i.e.* we allow real words as well as random combinations of letters).

```shell
> test wordParser "apple"
"apple"
> test wordParser "mmzk"
"mmzk"
> test wordParser "mmzk1526"
Syntax error at row 1 col 5:
  Unexpected '1'.
  Expecting <end of input>.
```

In the last test, the parser expects "end-of-input" because it treats "mmzk" as a word and expects nothing to follow.

We are now in the position to build the Wordlist parser. A Wordlist is a bunch of words separated by commas, and luckily, we have a built-in combinator that captures the exact behaviour:

```Haskell
wordlistParser :: Parser [String]
wordlistParser = sepBy1 (L.char ',') wordParser
```

The function `sepBy1` is another parser combinator. It takes two parsers, the first one is the separator (in our case, it is `L.char ','`, which parses exacly one comma), and the second one is the "content", which is `wordParser`[^1]. It then starts by parsing with the content parser, then the separator and the content parser, then the separator and the content parser again, until no more separator is found. The "1" in `sepBy1` means it requires at least one content (so an empty Wordlist is not allowed).

For example, assume the input is "apple,banana", then `wordlistParser` will first parse a word using `wordParser` (which is "apple"), then the first comma is consumed by the separator and the second word is parsed by `wordParser`, then no separator is found, and the parser terminates, producing `["apple", "banana"]`.

```shell
> test wordlistParser "apple,banana"
["apple","banana"]
> test wordlistParser "apple"
["apple"]
> test wordlistParser "apple,banana,1"
Syntax error at row 1 col 13:
  Unexpected ','.
  Expecting <end of input>.
```

The last test shows how the parser will react when `wordParser` detects unexpected characters: the parser "backtracks" to the point where a valid Wordlist is detected. More specifically, in the case of "apple,banana,1", it is apparent that the problem is with '1' since it is not a word, but the parser recognises "apple,banana" as a valid Wordlist and leaves ",1" behind, only to be picked up by `L.eof`. Therefore, the error message always claims expecting "end of input".

The reason behind such a phenomenon is that our parser eagerly backtracks by default, in contrast to most parser combinator libraries which do not backtrack at all unless specified otherwise. In our example, while such design allows us to pick up the longest prefix that abides the rules of Wordlist (which is useful in many cases), it often backtracks away from the spot where the actual error is detected.

### Pruning
To prevent this and pin-point the error Ground Zero, we may use the function `prune`, which prevents the parser to backtrack before the current location. We may redefine the `wordListParser` as following:

```Haskell
wordlistParser :: Parser [String]
wordlistParser = sepBy1 (lexer $ L.char ',') (prune >> wordParser)
```

Before explaining how it works, let us try the same examples:

```shell
> test wordlistParser "apple,banana"
["apple","banana"]
> test wordlistParser "apple"
["apple"]
> test wordlistParser "apple,banana,1"
Syntax error at row 1 col 14:
  Unexpected '1'.
  Expecting letter.
```

As we can see, it makes no difference with the valid inputs, but for the last one, it successfully locates the error at the digit '1'.

When parsing "apple,banana,1", `wordlistParser` first parses a word (namely "apple"), then a separator followed by a word (namely ",banana"). Now it reaches the second comma:

```txt
apple,banana,1
            ^
```

At this point, it tries to parse another separator followed by another word. If this operation fails, it would backtrack to the current position (the second comma) and return what it had so far (namely `["apple", "banana"]`):

```txt
apple,banana,1
            ^
            |
            backtrack position (if fails)
```

It then successfully parses the comma with `L.char ','`, reaching the digit '1', and continueing to parse using `prune >> wordParser`:

```txt
apple,banana,1
            |^
            |
            backtrack position
```

The function `prune` is then invoked, marking the current location (the digit '1') as the limit of backstracking. It then attempts to parse it as a letter and fails. At this point, it would backtrack to the second comma, but it is before the "pruned location", thus the backtrack is rejected, and an error is raised on the spot.

Pruning is often useful when a parser may be invoked many times by combinators such as `some` and `many`, since most of the time if an error occurs within such a combinator, there is no point to backtrack.

### Dealing with Spaces
There is one more problem: we do not allow spaces between commas and words, for example, `wordlistParser` does not accept "apple, banana" and would complain about the space:

```shell
> test wordlistParser "apple, banana"
Syntax error at row 1 col 7:
  Unexpected ' '.
  Expecting letter.
```

Apparently, it is natural to ignore these spaces, which is our objective in this section. To consume trailing spaces, we first need to define the "space parser" which "eats" extra spaces. There are many different definitions of what should be counted as "space". For instance, we may choose to only consider blankspace ' ' as space and do not include tabs and newlines, or choose to ignore all Unicode characters that are categorised as "space" (including blankspaces, tabs, newlines, vertical tabs, non-breaking blankspaces *etc.*). Here we choose the latter definition.

Again, the lexer module presents a built-in that does the job for us. The function `space` parses a single space character, and we can combine it with the combinator `many` to parse 0, 1, or more consecutive spaces. Finally, we need to make the parser aware of our space parser via the function `setSpaceParser`. In short, we replace the first line of the definition of `test` with:

```Haskell
test parser inputStr = putStrLn $ case parse (setSpaceParser (many L.space) >> parser <* L.eof) inputStr of
```

It means before we even use the parser, we set the space parser to `many L.space` which consumes any number of space characters.

Next, we modify `wordlistParser` as following:

```Haskell
wordlistParser = sepBy1 (lexer (L.char ',')) (prune >> lexer wordParser)
```

The combinator `lexer`[^2] runs its argument parser before invoking the space parser to consume any trailing spaces. Now any spaces after a comma or a word will be ignored:

```shell
> test wordlistParser "apple, banana"
["apple","banana"]
> test wordlistParser "apple, banana  , \t\ncherry  "
["apple","banana","cherry"]
> test wordlistParser " apple, banana"
Syntax error at row 1 col 1:
  Unexpected ' '.
  Expecting letter.
```

The tests above reveal one caveat: the parser still does not ignore any leading spaces. This is not unexpected, since so far we only ignore spaces after a token. The fix is also very easy: at the start, we just need to run `lexer` on a dummy parser that does not do anything, so we modify the definition of `test` again:

```Haskell
test parser inputStr = putStrLn $ case parse (setSpaceParser (many L.space) >> lexer (pure ()) >> parser <* L.eof) inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err
```

Here, `pure ()` is such a dummy parser, and `lexer (pure ())` does the same thing as our space parser (which is `many L.space`):

```shell
> test wordlistParser " apple, banana"
["apple","banana"]
```

In fact, the boilerplate of "setting the space parser -> use the space parser to ignore leading spaces -> run main parser -> reject extraneous inputs" is so common that we have a combinator that does exactly the same thing. We can write:

```Haskell
test parser inputStr = putStrLn $ case parse (L.wrapper (many L.space) parser) inputStr of
  Right a  -> show a
  Left err -> renderErrBundleAsStr err
```

### Letter Range
When `L.alpha` determines if a character is a letter, it is using its Unicode General Category, in other words, a character is considered as a letter if it can be interpreted so in any language. For example, the Chinese Character '皝' and the Japanese Kana 'ほ' are both considered as letters:

```shell
> test wordlistParser "献生不辰, 身播国屯, 终我四百, 永作虞宾"
["\29486\29983\19981\36784","\36523\25773\22269\23663","\32456\25105\22235\30334","\27704\20316\34398\23486"]
```

Note that the results are the codepoints of these characters. Using `putStrLn` on any of those would result in the original verse:

```shell
> putStrLn "\29486\29983\19981\36784"
献生不辰
```

Now assume we only want Latin letters in our word list. Notice that there is an ASCII module `MMZK.BSParser.ASCII` which also exports the function `alpha`, but it is not recommended unless we are certain that the input is fully comprised of ASCII-only characters. Instead, we can use the operator `(<&>)` to add constraints on the parsers. Replace the definition of `letterParser` with the following:

```Haskell
letterParser = L.alpha <&> [L.ascii]
```

Now, `letterParser` parses any letter **under the condition that it is also an ASCII character**. The second argument of `(<&>)` is a list, so we can pass in multiple constraints if necessary.

```shell
> test wordlistParser "献生不辰, 身播国屯, 终我四百, 永作虞宾"
Syntax error at row 1 col 1:
  Unexpected '\29486'.
  Expecting ascii char.
```

In practice, however, it is best to avoid using `(<&>)` since it need to run all constraints first before actually parsing. If possible, we'd better create a custom parser that parses the character in one go. In our case, we can use the function `satisfy` from the lexer module, which takes a predicate on characters and only parses successfully if the current character satisfies the predicate:

```Haskell
letterParser = L.satisfy (\ch -> isAlpha ch && isAscii ch)
```

Here, we are parsing a character that is both a letter and belongs to the ASCII character set. The behaviour should be (almost) identical:

```shell
> test wordlistParser "献生不辰, 身播国屯, 终我四百, 永作虞宾"
Syntax error at row 1 col 1:
  Unexpected '\29486'.
```

Note that there is no "expecting" message anymore since the parser cannot automatically generate the list of expected characters based on the predicate. In this case, we can add this piece of information manually via the operator `(<?>)`:

```Haskell
letterParser = L.satisfy (\ch -> isAlpha ch && isAscii ch) <?> ["ascii letter"]
```

Now when an error occurs in `letterParser`, it will say "Expecting ascii letter":

```shell
> test wordlistParser "献生不辰, 身播国屯, 终我四百, 永作虞宾"
Syntax error at row 1 col 1:
  Unexpected '\29486'.
  Expecting ascii letter.
```

Before we finish the tutorial, let's add more niche to the definition of "word". Let's say we allow words to contain dashes "-" and apostrophes "'",they can neither be the first or the last character nor appear consecutively, *e.g.* "I'm-MMZK" is allowed but "-a-" and "I'-m" are not.

Such ruke can be expressed succinctly as a Regex Expression: `[A-Za-z]+([-'][A-Za-z]+)*`, which means it starts with a least one letter, then it has zero or more groups of "apostrophe/dash followed by at least one letter". Here is how we could express it with the parser combinators by modifying `wordParser`:

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

It is a lot more complicated than before, but we will go through each construct slowly. Let's start from `groupParser`, which parses a symbol followed by at least one letter. The symbol is parsed with `choice [L.char '-', L.char '\'']`. The combinator `choice` accepts a list of parsers, it will try parser from left to right until one of them succeeds or all of them fails [^3].

In `wordParser`, it at first parses a list of letters using `some letterParser` (which is identical to the old `wordParser`), putting the result in `pureLetters`. Then we apply `groupParser` many times. Note that here we used `manyS` instead of `many`, this is because `groupParser` already returns a `String`, and `many groupParser` would return a list of `String`. The combinator `manyS` is similar to `many`, expect it "concatenates" the result. Finally, we return the entire word by concatenating the initial letter part and the (possibly empty) symbol groups.

```shell
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

### Conclusion
In this tutorial, we utilised some of the basic built-in combinators to implement a simple Wordlist parser. Of course, this is just the tip of the iceberg. In the following sections, we will introduce recursive descent, error handling & recovery, binary stream parsing, and combination with the `State` and `IO` monads. It will be enough to build a decent parser for a fairly complicated grammar!

In fact, there are a few more things we can doregarding the Wordlist example, but since is already quite long, we will split them into a separate one in the section below.

## Examples
TODO

## Documentation
TODO

[^1]: Note that the order of arguments is the opposite with the same function in the `megaparsec` package. It is designed so for more convenient partial application.

[^2]: Although `lexer` belongs to the lexer module `MMZK.BSParser.Lexer`, it is also exposed in the main module `MMZK.BSParser` since it's widely used.

[^3]: In most parser combinator libraries, `choice` (or its equivalent) does not allow backtracking, *i.e.* if a parser consumes input before failing, it would not go back to the start and try with the next parser, instead it would fail immediately. Here, however, we always backtrack eagerly unless we specifically forbid it with `prune`. See [pruning](#Pruning).
