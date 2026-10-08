{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2026
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@drexel.edu>
--
-- Stability   :  experimental
-- Portability :  non-portable

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
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef,
                   atomicModifyIORef',
                   modifyIORef',
                   atomicModifyIORef,
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

-- |The 'MonadRef' type class abstracts over the details of manipulating
-- references, allowing one to write code that uses references and can operate
-- in any monad that supports reference operations.

class (Monad m) => MonadRef m where
    type Ref m :: Type -> Type

    -- |Create a new reference
    newRef    :: a -> m (Ref m a)
    -- |Read the value of a reference
    readRef   :: Ref m a -> m a
    -- |Write a new value to a reference
    writeRef  :: Ref m a -> a -> m ()
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
class (MonadRef m) => MonadAtomicRef m where
    -- |Atomically mutate the contents of a reference
    atomicModifyRef :: Ref m a -> (a -> (a, b)) -> m b
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

instance MonadAtomicRef IO where
    atomicModifyRef = atomicModifyIORef
    atomicModifyRef' = atomicModifyIORef'

-- An ST computation cannot fork, so no other thread can write the reference
-- between the read and the write. This does not hold for an STRef RealWorld
-- that threads share through stToIO.
instance MonadAtomicRef (ST s) where
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
    atomicModifyRef r f = do x <- readRef r
                             let (x', y) = f x
                             writeRef r x'
                             return y

instance MonadAtomicRef m => MonadAtomicRef (ContT r m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance (Error e, MonadAtomicRef m) => MonadAtomicRef (ErrorT e m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance MonadAtomicRef m => MonadAtomicRef (ExceptT e m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (IdentityT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if !MIN_VERSION_transformers(0,6,0)
instance MonadAtomicRef m => MonadAtomicRef (ListT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance MonadAtomicRef m => MonadAtomicRef (MaybeT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (ReaderT r m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (Lazy.StateT s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (Strict.StateT s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Lazy.WriterT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Strict.WriterT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
