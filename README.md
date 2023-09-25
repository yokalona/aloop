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

**Examples**

| form                      | argument  | argument  | argument | explanation                                                     |
|---------------------------|-----------|-----------|----------|-----------------------------------------------------------------|
| -&#124;                   | \<arg>    | \<index>  | \<form>  | move `arg` to `+index` in `form`                                |        
| -&#124;&#124;             | \<index>  | \<form>   | \<arg>   | move `arg` to `+index` in `form`                                |
| &#124;-                   | \<arg>    | \<index>  | \<form>  | move `arg` to `-index` in `form`                                |
| &#124;&#124;-             | \<index>  | \<form>   | \<arg>   | move `arg` to `-index` in `form`                                |
| &#124;-&#124;             | \<index'> | \<index"> | \<form>  | swap `+index'` with `+index"` in `form`                         |
| &#124;-&#124;&#124;       | \<index'> | \<index"> | \<form>  | swap `+index'` with `-index"` in `form`                         |
| &#124;&#124;-&#124;       | \<index'> | \<index"> | \<form>  | swap `-index'` with `+index"` in `form`                         |
| &#124;&#124;-&#124;&#124; | \<index'> | \<index"> | \<form>  | swap `-index'` with `-index"` in `form`                         |
| &#124;->                  | \<index>  | \<form>   |          | swap `+index` with first arg of `form`                          |
| &#124;->>                 | \<index>  | \<form>   |          | swap `+index` with last arg of `form`                           |
| &#124;&#124;->            | \<index>  | \<form>   |          | swap `-index` with first arg of `form`                          |
| &#124;&#124;->>           | \<index>  | \<form>   |          | swap `-index` with last arg of `form`                           |
| \<-&#124;                 | \<index>  | \<form>   |          | swap `+index` with first arg of `form`                          |
| \<\<-&#124;               | \<index>  | \<form>   |          | swap `+index` with last arg of `form`                           |
| \<-&#124;&#124;           | \<index>  | \<form>   |          | swap `-index` with first arg of `form`                          |
| \<\<-&#124;&#124;         | \<index>  | \<form>   |          | swap `-index` with last arg of `form`                           |
| \<\<->                    | \<form>   |           |          | swap last with first arg of `form`                              |
| \<->>                     | \<form>   |           |          | swap last with first arg of `form`                              |
| over-&#124;               | \<arg>    | \<index>  | \<form>  | execute over `form` substituting `arg` at the position `+index` |
| over-&#124;&#124;         | \<form>   | \<index>  | \<arg>   | execute over `form` substituting `arg` at the position `+index` |
| over->                    | \<arg>    | \<form>   |          | execute over `form` substituting `arg` at the first position    |
| over->>                   | \<form>   | \<form>   |          | execute over `form` substituting `arg` at the last position     |
| &#124;-over               | \<arg>    | \<index>  | \<form>  | execute over `form` substituting `arg` at the position `-index` |
| &#124;&#124;-over         | \<form>   | \<index>  | \<arg>   | execute over `form` substituting `arg` at the position `-index` |
| \<-over                   | \<arg>    | \<form>   |          | execute over `form` substituting `arg` at the first position    |
| \<\<-over                 | \<form>   | \<form>   |          | execute over `form` substituting `arg` at the last position     |
