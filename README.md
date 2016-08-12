# `safepath`

The safe paths intends to be a replacement for `System.FilePath`.

## Consistency with `System.FilePath`

Many functions have the same names as the functions in `System.FilePath`, but
consistently was abandoned where:

- Naming was not great
- Naming did not make sense in the safe version
- Functions did not correspond exactly to their safe versions

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

## Only Posix

Currently `safepath` only supports POSIX paths for two reasons:

- Paths are hard
- The author has never programmed for Windows.
