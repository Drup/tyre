# Change Log

## NEXT
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

## 0.1.1 (09 September 2016)
* Fix a bug with nested repetitions. Also avoid some copying of the original string.
* Add Tyre.execp

## 0.1 (11 August 2016)
First version :tada:
