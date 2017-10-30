{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Monad
    ( module Parameterized.Control.Applicative
    , PMonad(..)
    , (&>>=)
    , (&>>)
    , (&=<<)
    , (&>=>)
    , (&<=<)
    ) where

import Data.Kind
import Parameterized.Control.Applicative

-- | Parameterized version of Monad.
class PApplicative m t u v => PMonad (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) where
    -- | Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    pbind :: m t a -> (a -> m u b) -> m v b

-- | Sequentially compose two actions, passing any value produced by the first as an argument to the second.
(&>>=) :: PMonad m t u v => m t a -> (a -> m u b) -> m v b
(&>>=) = pbind
infixl 1 &>>=
infixl 1 `pbind`

(&>>) :: PMonad m t u v => m t a -> m u b -> m v b
m &>> k = m &>>= \_ -> k
infixl 1 &>>

-- | Same as '&>>=', but with the arguments interchanged.
(&=<<) :: PMonad m t u v => (a -> m u b) -> m t a -> m v b
f &=<< x = x &>>= f
infixr 1 &=<<

-- | Left-to-right Kleisli composition of monads.
(&>=>) :: (PMonad m t u v) => (a -> m t b) -> (b -> m u c) -> (a -> m v c)
f &>=> g = \x -> f x &>>= g
infixr 1 &>=>

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
(&<=<) :: (PMonad m t u v) => (b -> m u c) -> (a -> m t b) -> (a -> m v c)
(&<=<) = flip (&>=>)
infixr 1 &<=<

-- class (PAlternative m t u v, PMonad m t u v) => PMonadPlus m t u v

-- class PMZero (m :: k -> Type -> Type) where
--     pmezero :: m (PId m) a
