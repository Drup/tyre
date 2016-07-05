# Tyre [![Build Status](https://travis-ci.org/Drup/tyre.svg?branch=master)](https://travis-ci.org/Drup/tyre) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]

Tyre is a set of combinators to build type-safe regular expressions, allowing automatic extraction and modification of matched groups.

Tyre is bi-directional: a typed regular expressions can be used for parsing and unparsing. It also allows routing, by providing a list of regexs/routes and their handlers.

Tyre is pure OCaml and uses [re][].

Documentation is available [here][doc].

[re]: https://github.com/ocaml/ocaml-re
[doc]: https://drup.github.io/tyre/dev/Tyre.html
