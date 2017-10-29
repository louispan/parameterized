{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Parameterized.Control.Monad.Trans.State.Strict.StateSpec (main, spec) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Parameterized.Control.Monad.Trans.State.Strict
import Parameterized.Control.Monad
import Data.Diverse
import Data.Semigroup
import Test.Hspec


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec dicovery.
main :: IO ()
main = hspec spec

manyIntMaybeState :: StateT (Many '[Int]) Maybe String
manyIntMaybeState= do
    s <- get
    let r = fetch @Int s
    case r of
        0 -> empty
        r' -> do
           put $ replace @Int s (r' + 1)
           pure $ show r'

manyBoolMaybeState :: StateT (Many '[Bool]) Maybe String
manyBoolMaybeState = do
    s <- get
    let r = fetch @Bool s
    case r of
        False -> empty
        r' -> do
            put $ replace @Bool s False
            pure $ show r'

--------------------------------

spec :: Spec
spec = do

    describe "ManyState" $ do
        it "pappend with empty doesn't change the type of the parameter" $ do
            let r1 = ManyState manyIntMaybeState `pappend` pempty
                r2 = pempty `pappend` ManyState manyIntMaybeState
                -- the "parameter" doesn't change when `pappend` with empty
                r1' = runManyState r1 :: StateT (Many '[Int]) Maybe String
                r2' = runManyState r2 :: StateT (Many '[Int]) Maybe String
            -- functionality unchanged
            runStateT r1' (single (5 :: Int)) `shouldBe` Just ("5", single (6 :: Int))
            runStateT r2' (single (5 :: Int)) `shouldBe` Just ("5", single (6 :: Int))

        it "it can 'pappend' to combine state runners 'Alternative'ly" $ do
            let r = ManyState manyIntMaybeState `pappend`
                        ManyState manyBoolMaybeState
                -- the "parameter" is combined
                r' = runManyState r :: StateT (Many '[Int, Bool]) Maybe String
            -- functionality of both state in an Alternative fashion
            -- manyIntMaybeState suceeded
            runStateT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Just ("5", (6 :: Int) ./ True ./ nil)
            runStateT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Just ("5", (6 :: Int) ./ False ./ nil)
            -- manyBoolMaybeState suceeded
            runStateT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Just ("True", (0 :: Int) ./ False ./ nil)
            -- Both failed
            runStateT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing

        it "it can 'papply' to combine state runners 'Applicative'ly" $ do
            let r = (\a b -> a <> "." <> b) <$> ManyState manyIntMaybeState &<*>
                        ManyState manyBoolMaybeState
                -- the "parameter" is combined
                r' = runManyState r :: StateT (Many '[Int, Bool]) Maybe String
            -- functionality of both readers in an Applicative fashion
            runStateT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Just ("5.True", (6 :: Int) ./ False ./ nil)
            runStateT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runStateT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runStateT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing


        it "it can 'pbind' to combine state runners 'Monad'ically" $ do
            let r = ManyState manyIntMaybeState &>>= \a ->
                        case a of
                            "1" -> ManyState manyBoolMaybeState
                            _ -> empty
                -- the "parameter" is combined
                r' = runManyState r :: StateT (Many '[Int, Bool]) Maybe String
            -- functionality of both readers in an Monad fashion
            runStateT r' ( (1 :: Int) ./ True ./ nil) `shouldBe` Just ("True", (2 :: Int) ./ False ./ nil)
            runStateT r' ( (1 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runStateT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runStateT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runStateT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runStateT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing
