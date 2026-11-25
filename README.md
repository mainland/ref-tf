# The `ref-tf` Package  [![Hackage](https://img.shields.io/hackage/v/ref-tf.svg)](https://hackage.haskell.org/package/ref-tf) [![Actions Status: haskell-ci](https://github.com/mainland/ref-tf/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/ref-tf/actions?query=workflow%3Ahaskell-ci)

Provides the `MonadRef` and `MonadAtomicRef` type classes, which abstract over
the details of manipulating references, allowing one to write code that can
operate in the `IO`, `ST`, or `STM` monad, or in a monad transformer from the
`transformers` package over any of them.

## AI assistance

All contributions included in releases through version `0.5.0.1` were made
without AI assistance. AI tools were used in development, review, and release
preparation after that version, through version `0.6.0`. The maintainer remains
responsible for this release.
