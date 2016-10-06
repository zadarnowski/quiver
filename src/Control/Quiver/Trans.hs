-- | Module:    Control.Quiver.Trans
-- Description: Monad transformers for quiver processors
-- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module provides functions for hoisting stream processors
-- over the various well-known monad transformers into the corresponding
-- base monad, in a manner analogous to the tranformers' @run@ function.
--
-- This is particularly useful for composing quiver processors over
-- distinct “compatible” monads such as 'ReaderT' and 'StateT'.

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Control.Quiver.Trans (
  qRunErrorT,
  qRunExceptT,
  qRunMaybeT,
  qRunRWST, qRunLazyRWST, qRunStrictRWST,
  qRunReaderT,
  qRunStateT, qRunLazyStateT, qRunStrictStateT,
  qRunWriterT, qRunLazyWriterT, qRunStrictWriterT,
) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy      as Lazy
import Control.Monad.Trans.RWS.Strict    as Strict
import Control.Monad.Trans.State.Lazy    as Lazy
import Control.Monad.Trans.State.Strict  as Strict
import Control.Monad.Trans.Writer.Lazy   as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Quiver.Internal

-- | Hoists a stream processor from an 'ErrorT e m' monad into its base monad @m@.

qRunErrorT :: Functor m => P a a' b b' (ErrorT e m) r -> P a a' b b' m (Either e r)
qRunErrorT = loop
 where
  loop (Consume x k q) = consume x (loop . k) (qRunErrorT q)
  loop (Produce y k q) = produce y (loop . k) (qRunErrorT q)
  loop (Enclose f)     = enclose (either (deliver . Left) loop <$> runErrorT f)
  loop (Deliver r)     = deliver (Right r)

-- | Hoists a stream processor from an 'ExceptT e m' monad into its base monad @m@.

qRunExceptT :: Functor m => P a a' b b' (ExceptT e m) r -> P a a' b b' m (Either e r)
qRunExceptT = loop
 where
  loop (Consume x k q) = consume x (loop . k) (qRunExceptT q)
  loop (Produce y k q) = produce y (loop . k) (qRunExceptT q)
  loop (Enclose f)     = enclose (either (deliver . Left) loop <$> runExceptT f)
  loop (Deliver r)     = deliver (Right r)

-- | Hoists a stream processor from a 'MaybeT m' monad into its base monad @m@.

qRunMaybeT :: Functor m => P a a' b b' (MaybeT m) r -> P a a' b b' m (Maybe r)
qRunMaybeT = loop
 where
  loop (Consume x k q) = consume x (loop . k) (qRunMaybeT q)
  loop (Produce y k q) = produce y (loop . k) (qRunMaybeT q)
  loop (Enclose f)     = enclose (maybe (deliver Nothing) loop <$> runMaybeT f)
  loop (Deliver r)     = deliver (Just r)

-- | Hoists a stream processor from a lazy 'RWST r w s m' monad into its base monad @m@.

qRunRWST :: (Functor m, Monoid mw) => P a a' b b' (Lazy.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
qRunRWST = qRunLazyRWST

-- | Hoists a stream processor from a lazy 'RWST r w s m' monad into its base monad @m@.

qRunLazyRWST :: (Functor m, Monoid mw) => P a a' b b' (Lazy.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
qRunLazyRWST p mr ms = loop p
 where
  loop (Consume x k q) = consume x (loop . k) (qRunLazyRWST q mr ms)
  loop (Produce y k q) = produce y (loop . k) (qRunLazyRWST q mr ms)
  loop (Enclose f)     = enclose (run <$> Lazy.runRWST f mr ms)
  loop (Deliver r)     = deliver (r, ms, mempty)
  run ~(p', ms', mw)    = adj mw <$> qRunLazyRWST p' mr ms'
  adj mw ~(r, ms', mw') = (r, ms', mappend mw mw')

-- | Hoists a stream processor from a strict 'RWST r w s m' monad into its base monad @m@.

qRunStrictRWST :: (Functor m, Monoid mw) => P a a' b b' (Strict.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
qRunStrictRWST p mr ms = loop p
 where
  loop (Consume x k q) = consume x (loop . k) (qRunStrictRWST q mr ms)
  loop (Produce y k q) = produce y (loop . k) (qRunStrictRWST q mr ms)
  loop (Enclose f)     = enclose (run <$> Strict.runRWST f mr ms)
  loop (Deliver r)     = deliver (r, ms, mempty)
  run (p', ms', mw)    = adj mw <$> qRunStrictRWST p' mr ms'
  adj mw (r, ms', mw') = (r, ms', mappend mw mw')

-- | Hoists a stream processor from a 'ReaderT r m' monad into its base monad @m@.

qRunReaderT :: Functor m => P a a' b b' (ReaderT mr m) r -> mr -> P a a' b b' m r
qRunReaderT p mr = loop p
 where
  loop (Consume x k q) = consume x (loop . k) (qRunReaderT q mr)
  loop (Produce y k q) = produce y (loop . k) (qRunReaderT q mr)
  loop (Enclose f)     = enclose (loop <$> runReaderT f mr)
  loop (Deliver r)     = deliver r

-- | Hoists a stream processor from a lazy 'StateT s m' monad into its base monad @m@.

qRunStateT :: Functor m => P a a' b b' (Lazy.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
qRunStateT = qRunLazyStateT

-- | Hoists a stream processor from a lazy 'StateT s m' monad into its base monad @m@.

qRunLazyStateT :: Functor m => P a a' b b' (Lazy.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
qRunLazyStateT p ms = loop p
 where
  loop (Consume x k q) = consume x (loop . k) (qRunLazyStateT q ms)
  loop (Produce y k q) = produce y (loop . k) (qRunLazyStateT q ms)
  loop (Enclose f)     = enclose (run <$> Lazy.runStateT f ms)
  loop (Deliver r)     = deliver (r, ms)
  run ~(p', ms')       = qRunLazyStateT p' ms'

-- | Hoists a stream processor from a strict 'StateT s m' monad into its base monad @m@.

qRunStrictStateT :: Functor m => P a a' b b' (Strict.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
qRunStrictStateT p ms = loop p
 where
  loop (Consume x k q) = consume x (loop . k) (qRunStrictStateT q ms)
  loop (Produce y k q) = produce y (loop . k) (qRunStrictStateT q ms)
  loop (Enclose f)     = enclose (uncurry qRunStrictStateT <$> Strict.runStateT f ms)
  loop (Deliver r)     = deliver (r, ms)

-- | Hoists a stream processor from a lazy 'WriterT w m' monad into its base monad @m@.

qRunWriterT :: (Functor m, Monoid mw) => P a a' b b' (Lazy.WriterT mw m) r -> P a a' b b' m (r, mw)
qRunWriterT = qRunLazyWriterT

-- | Hoists a stream processor from a lazy 'WriterT w m' monad into its base monad @m@.

qRunLazyWriterT :: (Functor m, Monoid mw) => P a a' b b' (Lazy.WriterT mw m) r -> P a a' b b' m (r, mw)
qRunLazyWriterT = loop
 where
  loop (Consume x k q) = consume x (loop . k) (qRunLazyWriterT q)
  loop (Produce y k q) = produce y (loop . k) (qRunLazyWriterT q)
  loop (Enclose f)     = enclose (run <$> Lazy.runWriterT f)
  loop (Deliver r)     = deliver (r, mempty)
  run ~(p', mw)        = adj mw <$> qRunLazyWriterT p'
  adj mw ~(r, mw')     = (r, mappend mw mw')

-- | Hoists a stream processor from a strict 'WriterT w m' monad into its base monad @m@.

qRunStrictWriterT :: (Functor m, Monoid mw) => P a a' b b' (Strict.WriterT mw m) r -> P a a' b b' m (r, mw)
qRunStrictWriterT = loop
 where
  loop (Consume x k q) = consume x (loop . k) (qRunStrictWriterT q)
  loop (Produce y k q) = produce y (loop . k) (qRunStrictWriterT q)
  loop (Enclose f)     = enclose (run <$> Strict.runWriterT f)
  loop (Deliver r)     = deliver (r, mempty)
  run (p', mw)         = adj mw <$> qRunStrictWriterT p'
  adj mw (r, mw')      = (r, mappend mw mw')
