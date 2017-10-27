{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Monad
    ( module Parameterized.Control.Applicative
    , PMonad(..)
    , pbind
    , (&>>=)
    , (&>>)
    , (&=<<)
    , (&>=>)
    , (&<=<)
    , PMonadPlus
    ) where

import Data.Kind
import Parameterized.Type
import Parameterized.Control.Applicative

-- | Parameterized version of Monad.
class PApplicative m t u v => PMonad (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) where
    pbind' :: m t a -> (a -> m u b) -> m v b

-- | Allow you to call 'pbind'' on any type as long as it is an instance of 'IsPUnary'
pbind
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v)
    => x a -> (a -> y b) -> z b
pbind x k = fromPUnary (toPUnary x `pbind'` (toPUnary . k))

(&>>=)
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v)
    => x a -> (a -> y b) -> z b
(&>>=) = pbind
infixl 1 &>>=

(&>>)
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v)
    => x a -> y b -> z b
m &>> k = m &>>= \_ -> k
infixl 1 &>>

-- | Same as '&>>=', but with the arguments interchanged.
(&=<<) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v) => (a -> y b) -> x a -> z b
f &=<< x = x &>>= f
infixr 1 &=<<

-- | Left-to-right Kleisli composition of monads.
(&>=>)
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v)
    => (a -> x b) -> (b -> y c) -> (a -> z c)
f &>=> g = \x -> f x &>>= g
infixr 1 &>=>

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
(&<=<) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PMonad m t u v)
    => (b -> y c) -> (a -> x b) -> (a -> z c)
(&<=<) = flip (&>=>)
infixr 1 &<=<

type PMonadPlus m t u v = (PAlternative m t u v, PMonad m t u v)
