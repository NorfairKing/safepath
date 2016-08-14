# `safepath` [![Build Status](https://travis-ci.org/NorfairKing/safepath.svg?branch=master)](https://travis-ci.org/NorfairKing/safepath)


## A reply to `filepath`

The `safepath` library intends to be a replacement for the most common usage of
`System.FilePath`.

Should FilePath be an abstract data type?

The answer for this library is "no". While an abstract FilePath has some
advantages (mostly type safety), it also has some disadvantages:

> In Haskell the definition is type FilePath = String, and all file-orientated
> functions operate on this type alias, e.g. readFile/writeFile. Any abstract
> type would require wrappers for these functions or lots of casts between String
> and the abstraction.

This library will (eventually) have the 'wrappers' that this paragraph speaks
about.  It will even have some safer versions of some of the 'equivalent'
functions.

> It is not immediately obvious what a FilePath is, and what is just a pure
> String. For example, /path/file.ext is a FilePath. Is /? /path? path?
> file.ext? .ext? file?

This is true. FilePaths are hard. This library simply assumes that a filepath
can point to a file and that users of the library don't usually _want_ to use
things that would make programmers question if it should be classified as a
filepath.

> Often it is useful to represent invalid files, e.g. /foo/*.txt probably isn't
> an actual file, but a glob pattern. Other programs use foo//bar for globs,
> which is definitely not a file, but might want to be stored as a FilePath.

This is true, but it is not the intended use of this library (yet).
Glob patterns could also be an abstract data type, but they are not the same
type as filepaths in this library.

> Some programs use syntactic non-semantic details of the FilePath to change
> their behaviour. For example, foo, foo/ and foo/. are all similar, and refer
> to the same location on disk, but may behave differently when passed to
> command-line tools.

This is a good point, but then again, tools will usually make very reasonable
assumptions and if they don't, we can still resort back to `String`s that are
derived from safe paths.

> A useful step to introducing an abstract FilePath is to reduce the amount of
> manipulating FilePath values like lists. This library hopes to help in that
> effort.

Great! This one does too.


## More reasons for an opaque datatype

- We can make a type-level distinction between absolute and relative paths and
  encourage the usage of absolute paths everywhere.

- We can guarantee more safety than we can with raw `String`s.

- Now that there are a lot of tests, we can safely optimise the abstract
  datatype for faster operations, less memory use, etc... without disrupting
  usage.

- We discourage the use of `String` in the rest of the application that is
  being built.

## Consistency with `System.FilePath`

Many functions have the same names as the functions in `System.FilePath`, but
consistency was abandoned where:

- Naming was not great
- Naming did not make sense in the safe version
- Functions' types did not correspond exactly to their safe versions

Documentation will specify, for each function in `System.FilePath`, which
function they should use when migrating.

## Exact versus pragmatic functions

Sometimes assumptions are made (and documented) to make a function's type
easier to use.  In those case, you can usually find a function with a similar
name, only postfixed with `Exact` that has a more complicated type but does not
make these assumptions.

An example is `replaceBaseNameExact :: Path rel -> LastPathPiece -> Maybe (Path
rel)` versus `replaceBaseName :: Path rel -> LastPathPiece -> Path rel`. Both
are safe but `replaceBaseNameExact` will return Nothing if the given
`LastPathPiece` is empty.

## Contributions

Contributions to this project are very welcome in the form of pull-requests

Specifically, there is Low-hanging fruit in the form of:

- More Examples
- More tests

## Only Posix for now

Currently `safepath` only supports POSIX paths for two reasons:

- Paths are hard enough already
- The author has never programmed for other platforms.

Pull requests to add support for other platforms are very welcome as long as:

- They don't disrupt the API for POSIX
- The code is tested to the same degree
