{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Monoid
    ( module Parameterized.TypeLevel
    , module Parameterized.Data.Semigroup
    , PMempty(..)
    , PMonoid
    ) where

import Data.Kind
import Parameterized.TypeLevel
import Parameterized.Data.Semigroup

-- | Parameterized version of mempty in Monoid.
class PMempty (n :: k -> Type) where
    pmempty :: n (PId n)

-- | Parameterized version of Monoid.
type PMonoid n t u v = (PMempty n, PSemigroup n t u v)
