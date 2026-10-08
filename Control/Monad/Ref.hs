{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
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
-- 'MonadAtomicRef' adds 'atomicModifyRef' for references that several threads
-- may update. Use it instead of 'readRef' followed by 'writeRef' when another
-- thread could write between them. Updates in 'ST' are atomic only within a
-- single thread, as 'MonadAtomicRef' explains.

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
import qualified Data.STRef.Lazy as Lazy (modifySTRef,
                                          newSTRef,
                                          readSTRef,
                                          writeSTRef)

-- |The 'MonadRef' type class abstracts over the details of manipulating
-- references, allowing one to write code that uses references and can operate
-- in any monad that supports reference operations.

class (Monad m) => MonadRef m where
    -- |The type of references in the monad @m@. The instances for monad
    -- transformers in this module use the reference type of the underlying
    -- monad.
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

-- As in strict ST, these updates are atomic only within a single thread.
instance MonadAtomicRef (Lazy.ST s) where
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

#if MIN_VERSION_transformers(0,5,3)
instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (AccumT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

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

#if MIN_VERSION_transformers(0,5,6)
instance MonadAtomicRef m => MonadAtomicRef (CPS.RWST r w s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Lazy.RWST r w s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Strict.RWST r w s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

#if MIN_VERSION_transformers(0,5,3)
instance MonadAtomicRef m => MonadAtomicRef (SelectT r m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,3) */

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

#if MIN_VERSION_transformers(0,5,6)
instance MonadAtomicRef m => MonadAtomicRef (CPS.WriterT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
#endif /* MIN_VERSION_transformers(0,5,6) */
