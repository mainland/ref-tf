module Main (main) where

import           Control.Concurrent.STM     (atomically)
import           Control.Exception          (ErrorCall, displayException,
                                             evaluate, try)
import           Control.Monad.Ref
import           Control.Monad.ST           (runST, stToIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.List                  (isInfixOf)
import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertBool,
                                             assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "References"
    [ testCase "ST updates" $ runST updates @?= updated
    , testCase "IO updates" $ updates >>= (@?= updated)
    , testCase "STM updates" $ atomically updates >>= (@?= updated)
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
    return (value, lazy, strict)

-- | The result of 'updates' when every method works.
updated :: (Int, Int, Int)
updated = (30, 6, 10)

assertError :: String -> IO () -> Assertion
assertError message action = do
    result <- try action :: IO (Either ErrorCall ())
    case result of
        Left err -> assertBool (displayException err) (message `isInfixOf` displayException err)
        Right () -> assertFailure ("Expected ErrorCall: " ++ message)
