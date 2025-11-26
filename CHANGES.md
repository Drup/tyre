# Unreleased

* Introduce charsets: contrary to `Re`, they have a different type from regex.
* Type the difference between regexps that can be evaluated reversed and the
  ones that cannot: `(evaluable, 'a) Tyre.t` and `(non_evaluable, 'a) Tyre.t`.
* Introduce alias `type pattern` for `(non_evaluable, 'a) Tyre.t`.
* Introduce `val lift : ('a -> string) -> 'a pattern -> ('e, 'a) t` to transform
  a pattern into an expression by giving an explicit conversion function.
  Also `liftpp` that does the same with better performance by using `Format`.
* Introduce `val unlift : (evaluable, 'a) t -> 'a pattern`.
* Introduce `val either: ('e, 'a) Tyre.t -> ('e, 'b) Tyre.t -> ('e, ('a, 'b) Either.t) Tyre.t`.
* Change the type of `alt` to `(_, 'a) t -> (_, 'a) t -> 'a pattern`. Previous
  users of `alt` should switch to `either`.
* Introduce ``val alt_eval: ('a -> [`Left | `Right]) -> ('e, 'a) t -> ('e, 'a) t -> ('e, 'a) t``
  This has flat typing but is compatible with `eval`.
* Operators: `<|>` is `alt`, `<||>` is `either`.
* Introduce `val map : ('a -> 'b) -> (_, 'a) t -> 'b pattern` and its
  corresponding operators: `let+` and `<$>`.
* Introduce `(and+)` which is an alias of `seq`.
* Introduce `val app: ('e, 'a -> 'b) t -> ('e, 'a) t -> 'b pattern` and its
  corresponding operator `<*>`
* Introduce `val matched_string : (_, 'a) t -> (_, string) t` that discards the
  computed value and just return the string that was matched.
* Drop dependency on `Result` library. Stdlib is now used.
* Introduce `val rep_charset: Charset.t -> (_, string) t`, and shortcut
  `val rep_any: (_, string) t`.

# 0.5 (24 January 2020)

* Move to dune
* Avoid deprecate Re functions

# 0.4.1 (17 August 2018)

* Reverts the removal of Re.marks.
  This fixes various bugs in the previous version.

# 0.4 (06 August 2018)

* Move to dune
* Remove the need for Re.marks.
  This might open the way to alterative backends, such as JS regexs.
  See https://github.com/Drup/tyre/issues/1 for details.
* Use Seq instead of Gen. This is a breaking change.

# 0.3 (17 April 2017)

* Performance improvements.
* Fix the behavior of opt (Prefer eating input).
* Remove conv_fail and allow usual converters to fail with an exception.
* Add Tyre.all and Tyre.all_gen

# 0.2 (08 October 2016)
* Rename `<?>` to `<|>`
* Rename `<*>` to `<&>`
* Add the `str` and `char` combinators for constant patterns.
* Add the `blank` combinator.
* Add an Infix module.
* `Tyre.conv` is now separated into two combinators, `conv` which doesn't use
  an option, but is not allowed to fail, and `conv_fail` which allows failures.
* The prefix (`<*`) and suffix (`*>`) operators now accepts tyregexs on both
  sides. The old behavior can be recovered by combining with `Tyre.str`.
  This makes prefixstr/suffixstr (`**>`/`<**`) redundant, they are removed.
* The various list combinators now accept a tyregex as separator.
  The old behavior can be recovered by combining with `Tyre.str`.
* Add the `start` and `stop` combinators.
* The ~whole argument for compile and route is removed.
  tyregex don't match the whole string by default anymore.
  You can use `Tyre.whole_string` or `Tyre.start` and `Tyre.stop` instead.

# 0.1.1 (09 September 2016)
* Fix a bug with nested repetitions. Also avoid some copying of the original string.
* Add Tyre.execp

# 0.1 (11 August 2016)
First version :tada:
