{-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}

module Test.Data.Plain
    ( emptyList
    , listInt
    , nonEmptyListInt
    , nonEmptyListBool
    , nonEmptyListSingleton
    , singletonSequenceEqualBoundInt
    , nonEmptyListExplicit
    , nonEmptyIntSequence
    ) where

import Data.List.NonEmpty (NonEmpty (..))


emptyList :: [Int]
emptyList = []

listInt :: [Int]
listInt = [3, 1, 2]

nonEmptyListInt :: NonEmpty Int
nonEmptyListInt = [5, 10, 7]

nonEmptyListBool :: NonEmpty Bool
nonEmptyListBool = [True, False]

nonEmptyIntSequence :: NonEmpty Int
nonEmptyIntSequence = [1..10]

singletonSequenceEqualBoundInt :: NonEmpty Int
singletonSequenceEqualBoundInt = [1..1]


nonEmptyListSingleton :: NonEmpty Int
nonEmptyListSingleton = [42]

nonEmptyListExplicit :: NonEmpty Int
nonEmptyListExplicit = 42 :| [50, 100]
