{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2026
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@drexel.edu>
-- Portability :  non-portable
--
-- 'MonadRef' gives 'IORef' in 'IO', 'STRef' in 'ST', and 'TVar' in 'STM' one
-- interface and lifts it through the monad transformers of the @transformers@
-- package. The associated type 'Ref' names each monad's reference type, so one
-- definition works in all of them:
--
-- > counter :: MonadRef m => m Int
-- > counter = do
-- >     r <- newRef 0
-- >     modifyRef' r (+ 1)
-- >     readRef r
--
-- As @counter :: IO Int@ it uses an 'IORef', under @runST@ an 'STRef', and
-- under @atomically@ a 'TVar'. In a transformer stack such as @StateT s IO@,
-- 'Ref' is the reference type of the underlying monad.
--
-- 'MonadAtomicRef' adds 'atomicWriteRef' and 'atomicModifyRef' for references
-- that several threads may update. Use 'atomicModifyRef' instead of
-- 'readRef' followed by 'writeRef' when another thread could write between
-- them. Updates in 'ST' are atomic only within a single thread.

module Control.Monad.Ref (
    MonadRef(..),
    MonadAtomicRef(..)
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar,
                                    modifyTVar,
                                    modifyTVar',
                                    newTVar,
                                    readTVar,
                                    writeTVar)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as Lazy (ST)
#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum (AccumT)
#endif /* MIN_VERSION_transformers(0,5,3) */
import Control.Monad.Trans.Cont (ContT)
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.Error (ErrorT, Error)
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List (ListT)
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS as CPS (RWST)
#endif /* MIN_VERSION_transformers(0,5,6) */
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import Control.Monad.Trans.RWS.Strict as Strict (RWST)
#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Select (SelectT)
#endif /* MIN_VERSION_transformers(0,5,3) */
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as CPS (WriterT)
#endif /* MIN_VERSION_transformers(0,5,6) */
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.IORef (IORef,
                   atomicModifyIORef',
                   modifyIORef',
                   atomicModifyIORef,
                   atomicWriteIORef,
                   modifyIORef,
                   newIORef,
                   readIORef,
                   writeIORef)
import Data.Kind (Type)
import Data.STRef (STRef,
                   modifySTRef',
                   modifySTRef,
                   newSTRef,
                   readSTRef,
                   writeSTRef)
import qualified Data.STRef.Lazy as Lazy (modifySTRef,
                                          newSTRef,
                                          readSTRef,
                                          writeSTRef)

-- |The 'MonadRef' type class abstracts over the details of manipulating
-- references, allowing one to write code that uses references and can operate
-- in any monad that supports reference operations.
--
-- In a monad transformer over a 'MonadRef' monad, 'newRef', 'readRef', and
-- 'writeRef' default to lifting the underlying operations, so an instance
-- needs only its 'Ref' equation:
--
-- > instance MonadRef m => MonadRef (MyT m) where
-- >     type Ref (MyT m) = Ref m
--
-- The defaults apply only to a monad @t n@ in which @t@ is a 'MonadTrans', @n@
-- is a 'MonadRef' monad, and @'Ref' (t n)@ is @'Ref' n@. An instance for any
-- other monad, such as a base monad, must define 'newRef', 'readRef', and
-- 'writeRef'. The minimal complete definition that Haddock reports is empty
-- only because of these defaults.
--
-- 'modifyRef' and 'modifyRef'' default to 'readRef' followed by 'writeRef' in
-- every instance. Define them by lifting to use the underlying monad's own
-- versions, as the instances in this module do.

class (Monad m) => MonadRef m where
    -- |The type of references in the monad @m@. The instances for monad
    -- transformers in this module use the reference type of the underlying
    -- monad.
    type Ref m :: Type -> Type

    -- |Create a new reference
    newRef    :: a -> m (Ref m a)
    default newRef
        :: (m ~ t n, MonadTrans t, MonadRef n, Ref m ~ Ref n)
        => a -> m (Ref m a)
    newRef = lift . newRef
    -- |Read the value of a reference
    readRef   :: Ref m a -> m a
    default readRef
        :: (m ~ t n, MonadTrans t, MonadRef n, Ref m ~ Ref n)
        => Ref m a -> m a
    readRef = lift . readRef
    -- |Write a new value to a reference
    writeRef  :: Ref m a -> a -> m ()
    default writeRef
        :: (m ~ t n, MonadTrans t, MonadRef n, Ref m ~ Ref n)
        => Ref m a -> a -> m ()
    writeRef r = lift . writeRef r
    -- |Mutate the contents of a reference
    modifyRef :: Ref m a -> (a -> a) -> m ()
    modifyRef r f = readRef r >>= writeRef r . f
    -- |Strict version of 'modifyRef'
    modifyRef' :: Ref m a -> (a -> a) -> m ()
    modifyRef' r f = readRef r >>= \x -> let x' = f x in x' `seq` writeRef r x'

-- |The 'MonadAtomicRef' type class extends 'MonadRef' with atomic
-- modification. Concurrent updates to a reference cannot interleave between
-- reading its old value and writing the new one.
--
-- Updates in 'ST' are atomic only because an 'ST' computation runs in a single
-- thread. Threads that share an @'STRef' 'Control.Monad.ST.RealWorld'@ through
-- 'Control.Monad.ST.stToIO' can interleave their updates and lose some of
-- them. Share an 'IORef' among threads instead.
--
-- In a monad transformer, 'atomicModifyRef' defaults to lifting the underlying
-- operation, which keeps it atomic. 'atomicModifyRef'' defaults to
-- 'atomicModifyRef' with both results forced, so it is atomic wherever
-- 'atomicModifyRef' is. An instance for any other monad, such as a base monad,
-- must define 'atomicModifyRef'.
--
-- An instance that omits 'atomicModifyRef'' gets that generic definition, not
-- the underlying monad's own version. The generic definition stores the new
-- value before it forces it. When forcing the new value throws, the reference
-- is left holding a value that throws whenever it is forced, whereas
-- 'atomicModifyRef'' in 'ST' leaves the old value in the reference. Define
-- 'atomicModifyRef'' by lifting to keep the underlying monad's behavior, as
-- the instances in this module do.
class (MonadRef m) => MonadAtomicRef m where
    -- | Atomically replace the value in a reference. This operation leaves
    -- both the previous and replacement values unevaluated. In 'IO' it
    -- imposes the memory barrier of 'atomicWriteIORef'. In 'STM' the write
    -- participates in the enclosing transaction.
    --
    -- The default uses 'atomicModifyRef'. A transformer can lift
    -- 'atomicWriteRef' directly to use the underlying instance's own write.
    atomicWriteRef :: Ref m a -> a -> m ()
    atomicWriteRef r x = atomicModifyRef r (const (x, ()))

    -- |Atomically mutate the contents of a reference
    atomicModifyRef :: Ref m a -> (a -> (a, b)) -> m b
    default atomicModifyRef
        :: (m ~ t n, MonadTrans t, MonadAtomicRef n, Ref m ~ Ref n)
        => Ref m a -> (a -> (a, b)) -> m b
    atomicModifyRef r = lift . atomicModifyRef r
    -- |Strict version of 'atomicModifyRef'. This forces both the value stored
    -- in the reference and the returned value to weak head normal form.
    atomicModifyRef' :: Ref m a -> (a -> (a, b)) -> m b
    atomicModifyRef' r f = do
        b <- atomicModifyRef r
                (\x -> let (a, b) = f x
                        in (a, a `seq` b))
        b `seq` return b

instance MonadRef (ST s) where
    type Ref (ST s) = STRef s

    newRef     = newSTRef
    readRef    = readSTRef
    writeRef   = writeSTRef
    modifyRef  = modifySTRef
    modifyRef' = modifySTRef'

instance MonadRef (Lazy.ST s) where
    type Ref (Lazy.ST s) = STRef s

    newRef    = Lazy.newSTRef
    readRef   = Lazy.readSTRef
    writeRef  = Lazy.writeSTRef
    modifyRef = Lazy.modifySTRef

instance MonadRef IO where
    type Ref IO = IORef

    newRef     = newIORef
    readRef    = readIORef
    writeRef   = writeIORef
    modifyRef  = modifyIORef
    modifyRef' = modifyIORef'

instance MonadRef STM where
    type Ref STM = TVar

    newRef     = newTVar
    readRef    = readTVar
    writeRef   = writeTVar
    modifyRef  = modifyTVar
    modifyRef' = modifyTVar'

#if MIN_VERSION_transformers(0,5,3)
instance (Monoid w, MonadRef m) => MonadRef (AccumT w m) where
    type Ref (AccumT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

instance MonadRef m => MonadRef (ContT r m) where
    type Ref (ContT r m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance (Error e, MonadRef m) => MonadRef (ErrorT e m) where
    type Ref (ErrorT e m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance (MonadRef m) => MonadRef (ExceptT e m) where
    type Ref (ExceptT e m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (IdentityT m) where
    type Ref (IdentityT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance MonadRef m => MonadRef (ListT m) where
    type Ref (ListT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance MonadRef m => MonadRef (MaybeT m) where
    type Ref (MaybeT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (ReaderT r m) where
    type Ref (ReaderT r m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if MIN_VERSION_transformers(0,5,6)
instance MonadRef m => MonadRef (CPS.RWST r w s m) where
    type Ref (CPS.RWST r w s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */

instance (Monoid w, MonadRef m) => MonadRef (Lazy.RWST r w s m) where
    type Ref (Lazy.RWST r w s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Monoid w, MonadRef m) => MonadRef (Strict.RWST r w s m) where
    type Ref (Strict.RWST r w s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if MIN_VERSION_transformers(0,5,3)
instance MonadRef m => MonadRef (SelectT r m) where
    type Ref (SelectT r m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

instance MonadRef m => MonadRef (Lazy.StateT s m) where
    type Ref (Lazy.StateT s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (Strict.StateT s m) where
    type Ref (Strict.StateT s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Monoid w, MonadRef m) => MonadRef (Lazy.WriterT w m) where
    type Ref (Lazy.WriterT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Monoid w, MonadRef m) => MonadRef (Strict.WriterT w m) where
    type Ref (Strict.WriterT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if MIN_VERSION_transformers(0,5,6)
instance MonadRef m => MonadRef (CPS.WriterT w m) where
    type Ref (CPS.WriterT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */

instance MonadAtomicRef IO where
    atomicWriteRef   = atomicWriteIORef
    atomicModifyRef  = atomicModifyIORef
    atomicModifyRef' = atomicModifyIORef'

-- An ST computation cannot fork, so no other thread can write the reference
-- between the read and the write. This does not hold for an STRef RealWorld
-- that threads share through stToIO.
instance MonadAtomicRef (ST s) where
    atomicWriteRef = writeRef

    atomicModifyRef r f  = do
      v <- readRef r
      let (a, b) = f v
      writeRef r a
      return b
    atomicModifyRef' r f = do
      v <- readRef r
      let (a, b) = f v
      writeRef r $! a
      b `seq` return b

-- As in strict ST, these updates are atomic only within a single thread.
instance MonadAtomicRef (Lazy.ST s) where
    atomicWriteRef = writeRef

    atomicModifyRef r f  = do
      v <- readRef r
      let (a, b) = f v
      writeRef r a
      return b
    atomicModifyRef' r f = do
      v <- readRef r
      let (a, b) = f v
      writeRef r $! a
      b `seq` return b

instance MonadAtomicRef STM where
    atomicWriteRef = writeTVar

    atomicModifyRef r f = do x <- readRef r
                             let (x', y) = f x
                             writeRef r x'
                             return y

#if MIN_VERSION_transformers(0,5,3)
instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (AccumT w m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

instance MonadAtomicRef m => MonadAtomicRef (ContT r m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance (Error e, MonadAtomicRef m) => MonadAtomicRef (ErrorT e m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance MonadAtomicRef m => MonadAtomicRef (ExceptT e m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (IdentityT m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance MonadAtomicRef m => MonadAtomicRef (ListT m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance MonadAtomicRef m => MonadAtomicRef (MaybeT m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (ReaderT r m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if MIN_VERSION_transformers(0,5,6)
instance MonadAtomicRef m => MonadAtomicRef (CPS.RWST r w s m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Lazy.RWST r w s m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Strict.RWST r w s m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if MIN_VERSION_transformers(0,5,3)
instance MonadAtomicRef m => MonadAtomicRef (SelectT r m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

instance MonadAtomicRef m => MonadAtomicRef (Lazy.StateT s m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (Strict.StateT s m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Lazy.WriterT w m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Strict.WriterT w m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if MIN_VERSION_transformers(0,5,6)
instance MonadAtomicRef m => MonadAtomicRef (CPS.WriterT w m) where
    atomicWriteRef   r x = lift $ atomicWriteRef   r x
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */
