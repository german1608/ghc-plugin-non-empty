{-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.Data.Fail
    ( emptyNonEmpty
    , emptySequenceInt
    -- , emptySequenceNegativeInt
    -- , emptySequenceEqualBoundInt
    -- , arithmeticExpresionEvaluatesToEmpty
    ) where

import Data.List.NonEmpty (NonEmpty)


emptyNonEmpty :: NonEmpty Int
emptyNonEmpty = []

emptySequenceInt :: NonEmpty Int
emptySequenceInt = [1..0]

-- emptySequenceNegativeInt :: NonEmpty Int
-- emptySequenceNegativeInt = [-1..(-2)]


-- arithmeticExpresionEvaluatesToEmpty :: NonEmpty Int
-- arithmeticExpresionEvaluatesToEmpty = [(1 + 1)..1]
