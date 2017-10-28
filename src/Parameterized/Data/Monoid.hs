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
class PMempty (n :: k -> Type) (id :: k) where
    pmempty :: n id

-- | Parameterized version of Monoid.
type PMonoid n id t u v = (PMempty n id, PSemigroup n t u v)
