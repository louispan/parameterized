{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Pointed where

import Data.Kind
import Parameterized.Type

-- | Parameterized version of 'pure' in 'Applicative'
-- An instance of this should create a parameterized unary type
-- where the parameter is an identity in respect to 'Parameterized.Data.Applicative.papply''
class PPointed (m :: k -> Type -> Type) where
    ppure' :: a -> m (PId m) a

-- | Allow you to call 'ppure'' on any type as long as it is an instance of 'IsPUnary'
ppure :: (IsPUnary x m (PId m), PPointed m) => a -> x a
ppure = fromPUnary . ppure'
