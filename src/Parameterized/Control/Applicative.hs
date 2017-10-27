{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Control.Applicative
    ( module Parameterized.Type
    , module Parameterized.Control.Pointed
    , module Parameterized.Data.Functor
    , PApplicative(..)
    , papply
    , (&<*>)
    , (&*>)
    , (&<*)
    , pliftA
    , pliftA2
    , pliftA3
    , PEmpty(..)
    , PAlternative(..)
    , pempty
    , pappend
    , (&<|>)
    ) where

import Data.Kind
import Parameterized.Type
import Parameterized.Control.Pointed
import Parameterized.Data.Functor

-- | Parameterized version of 'ap' in 'Applicative'
class (Functor (m t), Functor (m u), Functor (m v), PPointed m) =>
      PApplicative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    papply' :: m t (a -> b) -> m u a -> m v b

-- | Sequential application.
papply
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PApplicative m t u v)
    => x (a -> b) -> y a -> z b
papply x y = fromPUnary (toPUnary x `papply'` toPUnary y)

-- | Sequential application.
(&<*>) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PApplicative m t u v) => x (a -> b) -> y a -> z b
(&<*>) = papply
infixl 4 &<*>

-- | Sequence actions, discarding the value of the first argument.
(&*>) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PApplicative m t u v) => x a -> y b -> z b
a1 &*> a2 = (id &<$ a1) &<*> a2
infixl 4 &<*

-- | Sequence actions, discarding the value of the second argument.
(&<*) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PApplicative m t u v) => x a -> y b -> z a
(&<*) = pliftA2 const
infixl 4 &*> -- , &<**>

-- | Lift a function to actions.
pliftA
    :: (IsPUnary x m (PId m), IsPUnary y m t, PApplicative m (PId m) t t)
    => (a -> b) -> y a -> y b
pliftA f x = ppure f `papply` x

-- | Lift a binary function to actions.
pliftA2
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PApplicative m t u v)
    => (a -> b -> c) -> x a -> y b -> z c
pliftA2 f x y = f `pmap` x `papply` y

-- | Lift a ternary function to actions.
pliftA3
    :: ( IsPUnary f m t
       , IsPUnary g m u
       , IsPUnary h m v
       , IsPUnary i m w
       , IsPUnary j m x
       , PApplicative m t u v
       , PApplicative m v w x
       )
    => (a -> b -> c -> d) -> f a -> g b -> i c -> j d
pliftA3 f a b c = pliftA2 f a b &<*> c

-- | Parameterized version of empty in 'Alternative'.
class PEmpty (m :: k -> Type -> Type) where
    -- | The identity of '&<|>'
    pempty' :: m (PId m) a

-- | Allow you to call 'pmempty'' on any type as long as it is an instance of 'IsPNullary'
pempty :: (IsPUnary x m (PId m), PEmpty m) => x a
pempty = fromPUnary pempty'

-- | Parameterized version of 'Alternative'
class PApplicative m t u v =>
      PAlternative (m :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    -- | An associative binary operation
    pappend' :: m t a -> m u a -> m v a


-- | Allow you to call 'pappend'' on any type as long as it is an instance of 'IsPNullary'
pappend
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PAlternative m t u v)
    => x a -> y a -> z a
pappend x y = fromPUnary (toPUnary x `pappend'` toPUnary y)

(&<|>)
    :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, PAlternative m t u v)
    => x a -> y a -> z a
(&<|>) = pappend
infixl 3 &<|>
