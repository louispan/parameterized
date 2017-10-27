{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Semigroup where

import Data.Kind
import Parameterized.Type

-- | Parameterized version of (<>) in Semigroup
-- If used in conjunction with 'Parameterized.Data.Empty', ie as a parameterized Monoid,
-- then the instance should follow the following laws:
--  * @pmempty' `pmappend'` x = x@
--  * @x `pmappend'` pempty' = x@
--  * @x `pmappend'` (y `pmappend'` z) = (x `pmappend'` y) `pmappend'` z@
class PSemigroup (n :: k -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    pmappend' :: n t -> n u -> n v

-- | Allow you to call 'pappend'' on any type as long as it is an instance of 'IsPNullary'
pmappend :: (IsPNullary x n t, IsPNullary y n u, IsPNullary z n v, PSemigroup n t u v) => x -> y -> z
pmappend x y = fromPNullary (toPNullary x `pmappend'` toPNullary y)

(&<>) :: (IsPNullary x n t, IsPNullary y n u, IsPNullary z n v, PSemigroup n t u v) => x -> y -> z
(&<>) = pmappend
infixr 6 &<>
