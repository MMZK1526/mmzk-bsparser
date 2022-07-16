# mmzk-bsparser
`mmzk-bsparser` is a Monadic parser combinator library aiming for paring plain texts (encoded in UTF-8), binary stream, or a mixture of both. It uses `ByteString` as the underlining input stream, thus enabling fast random-access/backtracking as well as easy switch between plain texts and binary streams. By default, it can parse `String`, strict `Text`, and `[Word8]` by converting them to and from `ByteString`.

The library is currently in its first stage of development, thus most of the functionality and documentation are not available yet.

For a quick example on how to install and use this library, check [here](#quickstart).

The library is one of my side-projects and is inspired by many other in functional programming languages, such as [megaparsec](https://hackage.haskell.org/package/megaparsecs) and [parsley](https://index.scala-lang.org/http4s/parsley). In particular, the author of `parsley` has given my teammates and me valuable advices regarding the design and usage of parser combinators during our compiler coursework. In parallel to its development, I am also planning to use it to make parsers for a number of different grammars, including JSON and BSON.

## Contents
TODO

## Installation
TODO

## Quickstart
TODO

## Examples
TODO

## Documentation
TODO
