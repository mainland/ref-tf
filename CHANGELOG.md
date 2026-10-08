# Changelog

## Unreleased

- Add a `MonadAtomicRef` instance for `ExceptT`. It lifts both operations and
  preserves their lazy and strict result behavior.
- Fix `atomicModifyRef'` in `ST` to force its returned value to weak head
  normal form, as documented, even when the caller discards that value. The
  stored value remains strict, and `atomicModifyRef` remains lazy.
- Add regression tests for reference updates in `ST`, `IO`, and `STM`, for
  strict result evaluation through `ReaderT`, and for a strict `ST` update
  whose function throws, which must leave the old value in the reference.
- Use `Data.Kind.Type` instead of `*` to avoid kind notation warnings.
- Drop support for GHC versions before 8.0. Test GHC 8.0.2 through 9.14.1 in
  GitHub Actions, and enable library warnings.
- Require `base >= 4.9` and `transformers >= 0.5`, matching the supported GHC
  8.0+ range. Use HTTPS for the source repository and include release
  documentation in archives.
- Document `MonadAtomicRef`, including that updates in `ST` are not atomic
  across threads that share an `STRef` through `stToIO`. Use the current
  maintainer address in the module header and link the package homepage.
