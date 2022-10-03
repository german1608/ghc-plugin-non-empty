{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (ErrorCall, TypeError)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)

import Test.Data.Fail as Test
import Test.Data.Overloaded as Test
import Test.Data.Plain as Test


main :: IO ()
main = hspec $ describe "GhcPluginNonEmpty.plugin" $ do
    describe "Plain" $ do
        it "doesn't change ordinary empty list" $
            Test.emptyList `shouldBe` []
        it "doesn't change ordinary non-empty list" $
            Test.listInt `shouldBe` [3, 1, 2]
        it "correctly converts to non-empty int list" $
            Test.nonEmptyListInt `shouldBe` 5 :| [10, 7]
        it "correctly converts to non-empty bool list" $
            Test.nonEmptyListBool `shouldBe` True :| [False]
        it "correctly converts singleton non-empty int list" $
            Test.nonEmptyListSingleton `shouldBe` 42 :| []
        it "doesn't change explicit non-empty lists" $
            Test.nonEmptyListExplicit `shouldBe` 42 :| [50, 100]
        it "correctly converts non-empty int sequence list" $
            Test.nonEmptyIntSequence `shouldBe` 1 :| [2 .. 10]
        it "correctly converts non-empty int sequence with equal bounds to list" $
            Test.singletonSequenceEqualBoundInt `shouldBe` 1 :| []

    describe "Fail" $ do
        it "compiler error on trying to convert empty list to non-empty" $ testTypeError Test.emptyNonEmpty
        it "compiler error on trying to convert empty sequence from positive integers to non-empty"
            $ Test.emptySequenceInt `shouldBe` 1 :| []
        -- it "compiler error on trying to convert empty sequence from negative integers to non-empty"
        --     $ testTypeError Test.emptySequenceNegativeInt
        -- it "compiler error on trying to convert empty sequence from equal bounds to non-empty"
        --     $ testTypeError Test.emptySequenceEqualBoundInt
        -- it "compiler error on trying to convert empty sequence from arithmetic expression bounds to non-empty"
        --     $ testTypeError Test.arithmeticExpresionEvaluatesToEmpty

    describe "Overloaded" $ do
        it "doesn't change overloaded ordinary empty list" $
            Test.overloadedListEmpty `shouldBe` []
        it "doesn't change overloaded ordinary empty sequences" $
            Test.overloadedListEmptySequence `shouldBe` []
        it "doesn't change overloaded ordinary non-empty list" $
            Test.overloadedListInt `shouldBe` [3, 1, 2]
        it "converts overloaded NonEmpty list" $
            Test.overloadedNonEmptyInt `shouldBe` 15 :| [6, 7]
        it "runtime error on trying to convert empty list to non-empty" $ do
            let action = let !x = Test.overloadedEmptyNonEmpty in pure x
            action `shouldThrow` \(_ :: ErrorCall) -> True

  where
    testTypeError failingList = do
        let action = let !x = failingList in pure x
        action `shouldThrow` \(_ :: TypeError) -> True
