{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Applicative
    ( module Parameterized.TypeLevel
    , PPointed(..)
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
import Parameterized.TypeLevel

-- | Parameterized version of 'pure' in 'Applicative'
-- An instance of this should create a parameterized unary type
-- where the parameter is an identity in respect to 'Parameterized.Data.Applicative.papply''
class PPointed (m :: k -> Type -> Type) where
    -- | lift a value.
    ppure :: a -> m (PId m) a

-- | Parameterized version of 'ap' in 'Applicative'
class (Functor (m t), Functor (m u), Functor (m v), PPointed m) =>
      PApplicative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    -- | Sequential application.
    papply :: m t (a -> b) -> m u a -> m v b


-- | Sequential application.
(&<*>) :: (PApplicative m t u v) => m t (a -> b) -> m u a -> m v b
(&<*>) = papply
infixl 4 &<*>

-- | Sequence actions, discarding the value of the first argument.
(&*>) :: (PApplicative m t u v) => m t a -> m u b -> m v b
a1 &*> a2 = (id <$ a1) &<*> a2
infixl 4 &<*

-- | Sequence actions, discarding the value of the second argument.
(&<*) :: (PApplicative m t u v) => m t a -> m u b -> m v a
(&<*) = pliftA2 const
infixl 4 &*> -- , &<**>

-- | Lift a function to actions.
pliftA :: (PApplicative m (PId m) t t) => (a -> b) -> m t a -> m t b
pliftA f x = ppure f `papply` x

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
class PEmpty (m :: k -> Type -> Type) where
    -- | The identity of '&<|>'
    pempty :: m (PId m) a

-- | Parameterized version of 'Alternative'
class PApplicative m t u v =>
      PAlternative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    -- | An associative binary operation
    pappend :: m t a -> m u a -> m v a

(&<|>) :: (PAlternative m t u v) => m t a -> m u a -> m v a
(&<|>) = pappend
infixl 3 &<|>
