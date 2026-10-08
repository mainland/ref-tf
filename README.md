# The `ref-tf` Package  [![Hackage](https://img.shields.io/hackage/v/ref-tf.svg)](https://hackage.haskell.org/package/ref-tf) [![Actions Status: haskell-ci](https://github.com/mainland/ref-tf/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/ref-tf/actions?query=workflow%3Ahaskell-ci)

Provides the `MonadRef` and `MonadAtomicRef` type classes, which abstract over
the details of manipulating references, allowing one to write code that can
operate in the `IO`, `ST`, or `STM` monad, or in a monad transformer from the
`transformers` package over any of them.
