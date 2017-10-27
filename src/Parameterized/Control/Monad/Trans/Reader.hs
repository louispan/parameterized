{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameterized.Control.Monad.Trans.Reader where

import Control.Applicative
import Parameterized.Control.Applicative
import Control.Monad.Trans.Reader
import Data.Diverse

newtype ReaderChoice m r a = ReaderChoice
    { runReaderChoice :: ReaderT r m a
    } deriving (Functor)

instance Applicative m => Applicative (ReaderChoice m r) where
    pure = ReaderChoice . pure
    (ReaderChoice x) <*> (ReaderChoice y) = ReaderChoice (x <*> y)

instance Monad m => Monad (ReaderChoice m r) where
    (ReaderChoice x) >>= k = ReaderChoice $ x >>= runReaderChoice . k

type instance PId ReaderChoice = Which '[]

instance Applicative m => PPointed (ReaderChoice m) where
    ppure = pure

-- -- | Given a Reader that can read @Which a@, and another Reader that can read @Which b@
-- -- make a reader that can reader either @Which a@ or @Which b@
-- -- FIXME: This is nonsensical as it will probably be empty all the time.
-- instance ( Applicative m
--          , Alternative m
--          , Reinterpret' b c
--          , Reinterpret' a c
--          , c ~ AppendUnique a b
--          ) =>
--          PApplicative (ReaderChoice m) (Which a) (Which b) (Which c) where
--     (ReaderChoice (ReaderT f)) `papply'` (ReaderChoice (ReaderT x)) = ReaderChoice . ReaderT $ \c ->
--         case reinterpret' c of
--             Nothing -> empty
--             Just a -> case reinterpret' c of
--                           Nothing -> empty
--                           Just b -> f a <*> x b
