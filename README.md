# mmzk-bsparser
`mmzk-bsparser` is a Monadic parser combinator library aiming for paring plain texts (encoded in UTF-8), binary stream, or a mixture of both. It uses `ByteString` as the underlining input stream, thus enabling fast random-access/backtracking as well as easy switch between plain texts and binary streams. By default, it can parse `String`, strict `Text`, and `[Word8]` by converting them to and from `ByteString`.

The library is currently in its first stage of development, thus most of the functionality and documentation are not available yet.

For a quick example on how to install and use this library, check [here](#quickstart).

The library is one of my side-projects and is inspired by many other in functional programming languages, such as [megaparsec](https://hackage.haskell.org/package/megaparsecs) and [parsley](https://index.scala-lang.org/http4s/parsley). In particular, the author of `parsley` has given my teammates and me valuable advices regarding the design and usage of parser combinators during our compiler coursework. In parallel to its development, I am also planning to use it to make parsers for a number of different grammars, including JSON and BSON.

## Contents
TODO

## Installation
Since this library is not yet published, we need to install it manually before using it. Before installation, we assume that `ghc` and `cabal` are available ([here](https://www.haskell.org/cabal/) on how to install them).

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
    module Main where

    import MMZK.BSParser

    main :: IO ()
    main = print $ parse eof ""
    ```
5. Run `cabal run`, the library and the `Main` module would be built and the result should be `Just ()`, showing that the `eof` parser (which is equivalent to parsing an empty string when used alone) is successful. If we change the string to a non-empty one and run `cabal run` again, it should print out `Nothing`.

## Quickstart
TODO

## Examples
TODO

## Documentation
TODO
