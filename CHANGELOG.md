# Changelog

## Unreleased

- Add a `MonadAtomicRef` instance for `ExceptT`. It lifts both operations and
  preserves their lazy and strict result behavior.
- Add `MonadRef` and `MonadAtomicRef` instances for lazy and strict `RWST` and
  lazy `ST`, for `AccumT` and `SelectT` with `transformers` 0.5.3 or later, and
  for the CPS `WriterT` and `RWST` with `transformers` 0.5.6 or later.
- A package that defines its own instance of either class for one of these
  types, or its own `MonadAtomicRef` instance for `ExceptT`, now gets a
  duplicate instance error and must remove that instance.
- Give `newRef`, `readRef`, `writeRef`, and `atomicModifyRef` default
  definitions that lift through any `MonadTrans`, so a transformer's instance
  needs only its `Ref` equation. `modifyRef`, `modifyRef'`, and
  `atomicModifyRef'` keep their generic defaults, which do not use the
  underlying monad's own versions. An instance should define them by lifting.
  In particular, the default `atomicModifyRef'` over `ST` does not keep the
  old value when forcing the new value throws.
- An instance for a base monad must now define `newRef`, `readRef`, and
  `writeRef`, and its `MonadAtomicRef` instance must define `atomicModifyRef`,
  because the new defaults apply only to transformers. Earlier versions
  compiled an instance that omitted one of these methods with a warning. Such
  an instance no longer compiles.
- Fix `atomicModifyRef'` in `ST` to force its returned value to weak head
  normal form, as documented, even when the caller discards that value. The
  stored value remains strict, and `atomicModifyRef` remains lazy.
- Add regression tests for reference updates in `ST`, `IO`, and `STM`, for
  strict result evaluation through `ReaderT`, and for a strict `ST` update
  whose function throws, which must leave the old value in the reference.
  Test every method in each instance added here, and that the strict methods
  force their values in `STM`, in lazy `ST`, and in each new transformer.
- Use `Data.Kind.Type` instead of `*` to avoid kind notation warnings.
- Drop support for GHC versions before 8.0. Test GHC 8.0.2 through 9.14.1 in
  GitHub Actions, and enable library warnings.
- Require `base >= 4.9`, `transformers >= 0.5`, and `stm >= 2.3`, matching the
  supported GHC 8.0+ range. Use HTTPS for the source repository and include
  release documentation in archives.
- Implement `TVar` updates with `modifyTVar` and `modifyTVar'`.
- Replace the module description, which showed the header's stability and
  portability fields, with an overview and example, and describe `STM` and the
  transformer instances in the README and package description. Document
  `MonadAtomicRef`, including that updates in `ST` are not atomic across
  threads that share an `STRef` through `stToIO`. Use the current maintainer
  address in the module header and link the package homepage.
