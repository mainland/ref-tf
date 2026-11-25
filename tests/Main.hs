{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Main (main) where

import           Control.Concurrent.STM         (atomically)
import           Control.Exception              (ErrorCall, displayException,
                                                 evaluate, try)
import           Control.Monad.Ref
import           Control.Monad.ST               (runST, stToIO)
import qualified Control.Monad.ST.Lazy          as LazyST
import           Control.Monad.Trans.Class      (MonadTrans (..))
#if MIN_VERSION_transformers(0,5,3)
import           Control.Monad.Trans.Accum      (AccumT, runAccumT)
#endif
import           Control.Monad.Trans.Except     (runExceptT)
import           Control.Monad.Trans.Reader     (runReaderT)
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.RWS.CPS    as CPSRWS
#endif
import qualified Control.Monad.Trans.RWS.Lazy   as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
#if MIN_VERSION_transformers(0,5,3)
import           Control.Monad.Trans.Select     (SelectT, runSelectT)
#endif
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
#endif
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import           Data.List                      (isInfixOf)
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, assertBool,
                                                 assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "References"
    [ testCase "ST updates" $ runST updates @?= updated
    , testCase "IO updates" $ updates >>= (@?= updated)
    , testCase "ExceptT atomic updates" $
        (runExceptT updates :: IO (Either String (Int, Int, Int))) >>= (@?= Right updated)
    , testCase "ExceptT preserves lazy atomic results" $ do
        result <- runExceptT $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef ref (\x -> (x + 1, error "unused result" :: Int))
            readRef ref
        (result :: Either String Int) @?= Right 1
    , testCase "ExceptT preserves strict atomic results" $
        assertError "returned value" $ do
            _ <- runExceptT (do
                ref <- newRef (0 :: Int)
                _ <- atomicModifyRef' ref (\x -> (x + 1, error "returned value" :: Int))
                return ()) :: IO (Either String ())
            return ()
    , testCase "STM updates" $ atomically updates >>= (@?= updated)
    , strictness "STM" atomically
    , testCase "transformer defaults lift every operation" $
        runT updates >>= (@?= updated)
    , testCase "transformer default forces strict atomic results" $
        assertError "returned value" $ evaluate $ runST $ runT $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef' ref (\x -> (x + 1, error "returned value" :: Int))
            return ()
    , strictness "transformer defaults" (runT :: T IO () -> IO ())
    , testCase "base monad keeps the read-then-write modify defaults" $ do
        result <- runPlain $ do
            ref <- newRef (1 :: Int)
            modifyRef ref (+ 1)
            modifyRef' ref (* 10)
            readRef ref
        result @?= 20
    , testCase "lazy ST updates" $ LazyST.runST updates @?= updated
    , strictness "lazy ST" (stToIO . LazyST.lazyToStrictST)
    , testCase "lazy RWST updates" $ runLazyRWST updates >>= (@?= updated)
    , strictness "lazy RWST" runLazyRWST
    , testCase "strict RWST updates" $ runStrictRWST updates >>= (@?= updated)
    , strictness "strict RWST" runStrictRWST
#if MIN_VERSION_transformers(0,5,3)
    , testCase "AccumT updates" $ runAccum updates >>= (@?= updated)
    , strictness "AccumT" runAccum
    , testCase "SelectT updates" $ runSelect updates >>= (@?= updated)
    , strictness "SelectT" runSelect
#endif
#if MIN_VERSION_transformers(0,5,6)
    , testCase "CPS WriterT updates" $ runCPSWriter updates >>= (@?= updated)
    , strictness "CPS WriterT" runCPSWriter
    , testCase "CPS RWST updates" $ runCPSRWST updates >>= (@?= updated)
    , strictness "CPS RWST" runCPSRWST
#endif
    , testCase "strict ST update forces stored value" $
        assertError "stored value" $ evaluate $ runST $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef' ref (\_ -> (error "stored value", ()))
            return ()
    , testCase "strict ST update that throws keeps the old value" $ do
        ref <- stToIO $ newRef (0 :: Int)
        assertError "stored value" $
            stToIO $ atomicModifyRef' ref (\_ -> (error "stored value", ()))
        value <- stToIO $ readRef ref
        value @?= 0
    , testCase "strict ST update forces discarded result" $
        assertError "returned value" $ evaluate $ runST $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef' ref (\x -> (x + 1, error "returned value" :: Int))
            return ()
    , testCase "ST atomicModifyRef leaves discarded result unevaluated" $
        runST (do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef ref (\x -> (x + 1, error "returned value" :: Int))
            readRef ref) @?= 1
    , testCase "strict ST update forces only outer constructors" $
        runST (do
            ref <- newRef ([] :: [Int])
            _ <- atomicModifyRef' ref (\_ -> ([error "stored element"], [error "returned element" :: Int]))
            length <$> readRef ref) @?= 1
    , testCase "ReaderT preserves strict ST result evaluation" $
        assertError "returned value" $ evaluate $ runST $ flip runReaderT () $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef' ref (\x -> (x + 1, error "returned value" :: Int))
            return ()
    ]

-- | Call every method of both classes once. Each update changes the value in a
-- different way, so the result shows which of them ran.
updates :: MonadAtomicRef m => m (Int, Int, Int)
updates = do
    ref <- newRef (0 :: Int)
    writeRef ref 1
    modifyRef ref (+ 2)
    modifyRef' ref (* 2)
    lazy <- atomicModifyRef ref (\x -> (x + 4, x))
    strict <- atomicModifyRef' ref (\x -> (x * 3, x))
    value <- readRef ref
    atomicWriteRef ref (error "unused value")
    atomicWriteRef ref (value + 1)
    written <- readRef ref
    return (written, lazy, strict)

-- | The result of 'updates' when every method works.
updated :: (Int, Int, Int)
updated = (31, 6, 10)

-- | Check that the strict methods force the value they store and the value
-- they return, and that the lazy methods force neither. The runner must run
-- every action of the computation that it is given.
strictness :: MonadAtomicRef m => String -> (m () -> IO ()) -> TestTree
strictness name run = testGroup (name ++ " strictness")
    [ testCase "modifyRef' forces the stored value" $
        assertError "stored value" $ run $ do
            ref <- newRef (0 :: Int)
            modifyRef' ref (\_ -> error "stored value")
    , testCase "atomicModifyRef' forces the stored value" $
        assertError "stored value" $ run $ do
            ref <- newRef (0 :: Int)
            atomicModifyRef' ref (\_ -> (error "stored value", ()))
    , testCase "atomicModifyRef' forces the returned value" $
        assertError "returned value" $ run $ do
            ref <- newRef (0 :: Int)
            _ <- atomicModifyRef' ref
                (\x -> (x + 1, error "returned value" :: Int))
            return ()
    , testCase "modifyRef and atomicModifyRef force neither value" $ run $ do
        ref <- newRef (0 :: Int)
        modifyRef ref (\_ -> error "stored value")
        _ <- atomicModifyRef ref
            (\_ -> (error "stored value", error "returned value" :: Int))
        return ()
    ]

-- | A transformer whose instances rely on the lifting defaults.
newtype T m a = T { runT :: m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans T where
    lift = T

instance MonadRef m => MonadRef (T m) where
    type Ref (T m) = Ref m

instance MonadAtomicRef m => MonadAtomicRef (T m)

-- | A base monad whose instance relies on the read-then-write modify defaults.
newtype Plain a = Plain { runPlain :: IO a }
    deriving (Functor, Applicative, Monad)

instance MonadRef Plain where
    type Ref Plain = IORef

    newRef     = Plain . newIORef
    readRef    = Plain . readIORef
    writeRef r = Plain . writeIORef r

-- Run each new transformer over 'IO' with unit parameters.
runLazyRWST :: LazyRWS.RWST () () () IO a -> IO a
runLazyRWST m = fst3 <$> LazyRWS.runRWST m () ()

runStrictRWST :: StrictRWS.RWST () () () IO a -> IO a
runStrictRWST m = fst3 <$> StrictRWS.runRWST m () ()

#if MIN_VERSION_transformers(0,5,3)
runAccum :: AccumT () IO a -> IO a
runAccum m = fst <$> runAccumT m ()

runSelect :: SelectT () IO a -> IO a
runSelect m = runSelectT m (\_ -> return ())
#endif

#if MIN_VERSION_transformers(0,5,6)
runCPSWriter :: CPSWriter.WriterT () IO a -> IO a
runCPSWriter m = fst <$> CPSWriter.runWriterT m

runCPSRWST :: CPSRWS.RWST () () () IO a -> IO a
runCPSRWST m = fst3 <$> CPSRWS.runRWST m () ()
#endif

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

assertError :: String -> IO () -> Assertion
assertError message action = do
    result <- try action :: IO (Either ErrorCall ())
    case result of
        Left err -> assertBool (displayException err) (message `isInfixOf` displayException err)
        Right () -> assertFailure ("Expected ErrorCall: " ++ message)
