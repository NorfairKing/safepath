# `safepath`

The safe paths intends to be a replacement for `System.FilePath`.

## Consistency with `System.FilePath`

Many functions have the same names as the functions in `System.FilePath`, but consistently was abandoned where:

- Naming was not great
- Naming did not make sense in the safe version
- Functions did not correspond exactly to their safe versions

Documentation will specify, for each function in `System.FilePath`, which function they should use when migrating.

## Only Posix

Currently `safepath` only supports POSIX paths for two reasons:

- Paths are hard
- The author has never programmed for Windows.
