{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Parameterized.Data.Functor where

import Parameterized.Type

pmap :: (IsPUnary f m t, Functor (m t)) => (a -> b) -> f a -> f b
pmap f x = fromPUnary (f <$> toPUnary x)

(&<$>) :: (IsPUnary f m t, Functor (m t)) => (a -> b) -> f a -> f b
(&<$>) = pmap
infixl 4 &<$>

(&<$) :: (IsPUnary f m t, Functor (m t)) => a -> f b -> f a
(&<$) = pmap . const
infixl 4 &<$

(&$>) :: (IsPUnary f m t, Functor (m t)) => f a -> b -> f b
(&$>) = flip (&<$)
infixl 4 &$>

pvoid :: (IsPUnary f m t, Functor (m t)) => f a -> f ()
pvoid x = () &<$ x
