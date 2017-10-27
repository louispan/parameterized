{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Empty where

import Data.Kind
import Parameterized.Type

-- | Parameterized version of mempty in Monoid.
-- An instance of this should create a parameterized monotype
-- where the parameter is an identity in respect to 'Parameterized.Data.Semigroup.pappend''
class PEmpty (n :: k -> Type) where
    pempty' :: n (PId n)

-- | Allow you to call 'pempty'' on any type as long as it is an instance of 'IsPNullary'
pempty :: (IsPNullary x n (PId n), PEmpty n) => x
pempty = fromPNullary pempty'
