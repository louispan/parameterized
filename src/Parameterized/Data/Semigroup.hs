{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Semigroup where

import Data.Kind

-- | Parameterized version of (<>) in Semigroup
-- If used in conjunction with 'Parameterized.Data.Empty', ie as a parameterized Monoid,
-- then the instance should follow the following laws:
--  * @pmempty' `pmappend'` x = x@
--  * @x `pmappend'` pempty' = x@
--  * @x `pmappend'` (y `pmappend'` z) = (x `pmappend'` y) `pmappend'` z@
class PSemigroup (n :: k -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    pmappend :: n t -> n u -> n v

(&<>) :: (PSemigroup n t u v) => n t -> n u -> n v
(&<>) = pmappend
infixr 6 &<>
infixr 6 `pmappend`
