{-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}

{-# LANGUAGE OverloadedLists #-}

module Test.Data.Overloaded
   ( overloadedListEmpty
   , overloadedListEmptySequence
   , overloadedListInt
   , overloadedNonEmptyInt
   , overloadedEmptyNonEmpty
   ) where

import Data.List.NonEmpty (NonEmpty)

overloadedListEmpty :: [Int]
overloadedListEmpty = []

overloadedListEmptySequence :: [Int]
overloadedListEmptySequence = [10..0]

overloadedListInt :: [Int]
overloadedListInt = [3, 1, 2]

overloadedNonEmptyInt :: NonEmpty Int
overloadedNonEmptyInt = [15, 6, 7]

overloadedEmptyNonEmpty :: NonEmpty Int
overloadedEmptyNonEmpty = []
