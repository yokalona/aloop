# aloop

[![Clojure CI](https://github.com/yokalona/aloop/actions/workflows/clojure.yml/badge.svg)](https://github.com/yokalona/aloop/actions/workflows/clojure.yml)
[![Clojars Project](https://img.shields.io/clojars/v/io.github.yokalona/aloop.svg)](https://clojars.org/io.github.yokalona/aloop)

Set of useful macros and functions.

This project consists of macros that I use in my other projects.

Code notations of this and any other my project:

| Symbol      | Meaning                                                                                                                                                                                                   |
|-------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `-foo>`     | alias for `set foo`                                                                                                                                                                                       |
| `<foo-`     | alias for `get foo`                                                                                                                                                                                       |
| `-foo`      | private function                                                                                                                                                                                          |
| `foo!`      | state altering function                                                                                                                                                                                   |
| `foo!!`     | heavy state altering function                                                                                                                                                                             |
| `bar->baz`  | do action `bar` on `baz`, if `baz` is a collection - count index forward, i.e first element is 0, second is 1 and so on. Otherwise - count from back to front, i.e. last element is -1, second to last -2 |
| `bar->>baz` | do action `bar` on `baz`, if `baz` is a collection - count index backwards, i.e last element is -1, second to last is -2 and so on. Otherwise - count forward, i.e. first element is 0, second - 1        |
|             |                                                                                                                                                                                                           |



