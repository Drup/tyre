# Tyre [![Build Status](https://travis-ci.org/Drup/tyre.svg?branch=master)](https://travis-ci.org/Drup/tyre) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]

Tyre is a set of combinators to build type-safe regular expressions, allowing automatic extraction and modification of matched groups.

Tyre is bi-directional: a typed regular expressions can be used for parsing and unparsing. It also allows routing, by providing a list of regexs/routes and their handlers.

Tyre is pure OCaml and uses [re][]. To install it:

```
opam install tyre
```

Documentation is available [here][doc]. See also the [examples/](examples/) directory.
A primitive HTTP parser can be found in the [benchmark/](benchmark/) directory.

[re]: https://github.com/ocaml/ocaml-re
[doc]: https://drup.github.io/tyre/doc/dev/tyre/Tyre/index.html

```ocaml
# let dim = Tyre.( str"dim:" *> int <&> str"x" *> int ) ;;
val dim : (int * int) Tyre.t

# let dim_re = Tyre.compile dim ;;
val dim_re : (int * int) Tyre.re

# Tyre.exec dim_re "dim:3x4" ;;
- : (int * int, (int * int) Tyre.error) result = Result.Ok (3, 4)

# Tyre.eval dim (2, 5) ;;
- : string = "dim:2x5"
```

## Benchmark 

Some benchmarks are available under the [benchmark/](benchmark/) directory. 
The benchmark compares the parsing of HTTP requests using angstrom and various
tyre methods.
You can run them with:
```
./benchmark/data/replicate benchmark/data/http-requests.txt 100
jbuilder exec benchmark/benchmark_angstrom.exe -- -a
```
