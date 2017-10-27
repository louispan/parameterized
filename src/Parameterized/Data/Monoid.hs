{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Monoid
    ( module Parameterized.Type
    , module Parameterized.Data.Semigroup
    , PMempty(..)
    , PMonoid
    , pmempty
    ) where

import Data.Kind
import Parameterized.Type
import Parameterized.Data.Semigroup

-- | Parameterized version of mempty in Monoid.
class PMempty (n :: k -> Type) where
    pmempty' :: n (PId n)

-- | Parameterized version of Monoid.
type PMonoid n t u v = (PMempty n, PSemigroup n t u v)

-- | Allow you to call 'pmempty'' on any type as long as it is an instance of 'IsPNullary'
pmempty :: (IsPNullary x n (PId n), PMempty n) => x
pmempty = fromPNullary pmempty'
