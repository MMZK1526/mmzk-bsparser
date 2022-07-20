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
              ../mmzk-bsparser/
    ```
4. Now we can import the library. For example, in "app/Main.hs", we can write:
    ```Haskell
    main :: IO ()
    main = case parse L.eof "" of
        Right a -> print a
        Left _  -> putStrLn "has error"
    ```
5. Run `cabal run`, the library and the `Main` module would be built and the result should be `()`, showing that the `eof` parser (which is equivalent to parsing an empty string when used alone) is successful. If we change the string to a non-empty one and run `cabal run` again, it should print a `"has error"`.

In each of the [examples](#Examples), we will assume an empty playground as above and work from there.

## Quickstart
In this example, we will make a very simple parser that takes a "Wordlist" separated by commas and produces a list of words. For example, the input "apple, banana, cherry" would be parsed into the list `["apple", "banana", "cherry"]`. This is a very simple functionality that can be easily implemented without any kind of parser combinators, but it is nevertheless an easy example to demonstrate how those combinators work. We will introduce more complicated examples in the [next section](#examples).  

To follow this example, make sure that the ["playground" workspace](#installation) is ready.  

In "app/Main.hs", replace the existing code with the following:

```Haskell
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

In the last example, the parser expects "end-of-input" because it treats "mmzk" as a word and expects nothing to follow. Apparently, this is not the best error message and can lead to confusion. Later we will discuss how to make the error more informative.

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

The last example shows how the parser will react when `wordParser` detects unexpected characters: the parser "backtracks" to the point where a valid Wordlist is detected. More specifically, in the case of "apple,banana,1", it is apparent that the problem is with '1' since it is not a word, but the parser recognises "apple,banana" as a valid Wordlist and leaves ",1" behind, only to be picked up by `L.eof`. Therefore, the error message always claims expecting "end of input".

The reason behind such a phenomenon is that our parser eagerly backtracks by default, in contrast to most parser combinator libraries which do not backtrack at all unless specified otherwise. In our example, while such design allows us to pick up the longest prefix that abides the rules of Wordlist (which is useful in many cases), it often backtracks away from the spot where the actual error is detected.

To prevent this and pin-point the error Ground Zero, we may use the function `prune`, which stops prevents the parser to backtrack before the current location. We may redefined `wordlistParser` as the following:

```Haskell
wordlistParser :: Parser [String]
wordlistParser = sepBy1 (L.char ',') (prune >> wordParser)
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

The function `prune` is invoked, marking the current location (the digit '1') as the limit of backstracking. It then attempts to parse it as a letter and fails. At this point, it would backtrack to the second comma, but it is before the "pruned location", thus the backtrack is rejected, and an error is raised on the spot.

## Examples
TODO

## Documentation
TODO

[^1]: Note that the order of arguments is the opposite with the same function in the `megaparsec` package. It is designed so for more convenient partial application.
