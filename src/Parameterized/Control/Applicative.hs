{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Applicative
    ( PPointed(..)
    , PApplicative(..)
    , (&<*>)
    , (&*>)
    , (&<*)
    , pliftA
    , pliftA2
    , pliftA3
    , PEmpty(..)
    , PAlternative(..)
    , (&<|>)
    ) where

import Data.Kind

-- | Parameterized version of 'pure' in 'Applicative'
-- An instance of this should create a parameterized unary type
-- where the parameter is an identity in respect to 'papply'
class PPointed (m :: k -> Type -> Type) (id :: k) where
    -- | lift a value.
    ppure :: a -> m id a

-- | Parameterized version of 'ap' in 'Applicative'
-- NB. 'PPointed' cannot be made a superclass because type variable @id@ is not in scope.
class (Functor (m t), Functor (m u), Functor (m v)) =>
      PApplicative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    -- | Sequential application.
    papply :: m t (a -> b) -> m u a -> m v b


-- | Sequential application.
(&<*>) :: (PApplicative m t u v) => m t (a -> b) -> m u a -> m v b
(&<*>) = papply
infixl 4 &<*>
infixl 4 `papply`

-- | Sequence actions, discarding the value of the first argument.
(&*>) :: (PApplicative m t u v) => m t a -> m u b -> m v b
a1 &*> a2 = (id <$ a1) &<*> a2
infixl 4 &<*

-- | Sequence actions, discarding the value of the second argument.
(&<*) :: (PApplicative m t u v) => m t a -> m u b -> m v a
(&<*) = pliftA2 const
infixl 4 &*>

-- | Lift a function to actions.
pliftA :: (Functor (m t)) => (a -> b) -> m t a -> m t b
pliftA f x = f <$> x

-- | Lift a binary function to actions.
pliftA2 :: (PApplicative m t u v) => (a -> b -> c) -> m t a -> m u b -> m v c
pliftA2 f x y = (f <$> x) `papply` y

-- | Lift a ternary function to actions.
pliftA3
    :: ( PApplicative m t u v
       , PApplicative m v w x
       )
    => (a -> b -> c -> d) -> m t a -> m u b -> m w c -> m x d
pliftA3 f a b c = pliftA2 f a b &<*> c

-- | Parameterized version of empty in 'Alternative'.
-- An instance of this should create a parameterized unary type
-- where the parameter is an identity in respect to 'pappend'
class PEmpty (m :: k -> Type -> Type) (id :: k) | m -> id where
    -- | The identity of '&<|>'
    pempty :: m id a

-- | Parameterized version of 'Alternative'
-- NB. 'PEmpty' cannot be made a superclass because type variable @id@ will be ambiguous.
-- NB. PAlternative doensn't require 'PApplicative' as a superclass, because
-- Some things can be made instances of 'PAlternative' but not 'PApplicative'.
class PAlternative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    -- | An associative binary operation
    pappend :: m t a -> m u a -> m v a

(&<|>) :: (PAlternative m t u v) => m t a -> m u a -> m v a
(&<|>) = pappend
infixl 3 &<|>
infixl 3 `pappend`
