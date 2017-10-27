{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Parameterized.Type where

import Data.Kind

-- | Turns a parameter that is an identity.
type family PId (m :: k1) :: k2


-- | Converts a type to/from a "parameterized" type which is a monotype
-- after application of the "parameter" kind argument (eg. Monoid, Semigroup)
-- This is useful when there is only one meaningful parameterized version of a type,
-- but that type may not have the type arguments in the correct order.
-- This class enables calling "parameterized" combinators on those those types.
-- Note: the functional dependencies are injective both directions to help type inference.
class IsPNullary (x :: Type) (n :: k -> Type) (t :: k) | x -> n t, n t -> x where
    toPNullary :: x -> n t
    fromPNullary :: n t -> x

-- | Converts a type to/from a "parameterized" type which accepts one type argument,
-- after application of the "parameter" kind argument (eg. Functor, Monad)
-- This is useful when there is only one meaningful parameterized version of a type,
-- but that type may not have the type arguments in the correct order.
-- This class enables calling "parameterized" combinators on those those types.
-- Note: the functional dependencies are injective both directions to help type inference.
class IsPUnary x (m :: k -> Type -> Type) (t :: k) | x -> m t, m t -> x where
    toPUnary :: x a -> m t a
    fromPUnary :: m t a -> x a

-- | Get the first type of a type level tuple
-- This is useful for defining a newtype wrapper with a single "parameterized" type,
-- around a type with many "parameterized" type variables.
type family At0 t where
    At0 (x, _) = x
    At0 (x, _, _) = x
    At0 (x, _, _, _) = x
    At0 (x, _, _, _, _) = x
    At0 (x, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _, _, _) = x

-- | Get the second type of a type level tuple
type family At1 t where
    At1 (_, x) = x
    At1 (_, x, _) = x
    At1 (_, x, _, _) = x
    At1 (_, x, _, _, _) = x
    At1 (_, x, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _, _, _) = x

-- | Get the third type of a type level tuple
type family At2 t where
    At2 (_, _, x) = x
    At2 (_, _, x, _) = x
    At2 (_, _, x, _, _) = x
    At2 (_, _, x, _, _, _) = x
    At2 (_, _, x, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _, _, _) = x

-- | Get the fourth type of a type level tuple
type family At3 t where
    At3 (_, _, _, x) = x
    At3 (_, _, _, x, _) = x
    At3 (_, _, _, x, _, _) = x
    At3 (_, _, _, x, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _, _, _) = x

-- | Get the fifth type of a type level tuple
type family At4 t where
    At4 (_, _, _, _, x) = x
    At4 (_, _, _, _, x, _) = x
    At4 (_, _, _, _, x, _, _) = x
    At4 (_, _, _, _, x, _, _, _) = x
    At4 (_, _, _, _, x, _, _, _, _) = x
    At4 (_, _, _, _, x, _, _, _, _, _) = x

-- | Get the sixth type of a type level tuple
type family At5 t where
    At5 (_, _, _, _, _, x) = x
    At5 (_, _, _, _, _, x, _) = x
    At5 (_, _, _, _, _, x, _, _) = x
    At5 (_, _, _, _, _, x, _, _, _) = x
    At5 (_, _, _, _, _, x, _, _, _, _) = x

-- | Get the seventh type of a type level tuple
type family At6 t where
    At6 (_, _, _, _, _, _, x) = x
    At6 (_, _, _, _, _, _, x, _) = x
    At6 (_, _, _, _, _, _, x, _, _) = x
    At6 (_, _, _, _, _, _, x, _, _, _) = x

-- | Get the eigth type of a type level tuple
type family At7 t where
    At7 (_, _, _, _, _, _, _, x) = x
    At7 (_, _, _, _, _, _, _, x, _) = x
    At7 (_, _, _, _, _, _, _, x, _, _) = x

-- | Get the nineth type of a type level tuple
type family At8 t where
    At8 (_, _, _, _, _, _, _, _, x) = x
    At8 (_, _, _, _, _, _, _, _, x, _) = x

-- | Get the tenth type of a type level tuple
type family At9 t where
    At9 (_, _, _, _, _, _, _, _, _, x) = x
