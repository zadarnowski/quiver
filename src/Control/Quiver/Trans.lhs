> -- | Module:    Control.Quiver.Trans
> -- Description: Monad transformers for quiver processors
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module provides a definition of a /simple processor/
> -- with a unit request type and an unspecified acknowledgement
> -- type, together with a number of common combinators for their
> -- definitions.

> {-# LANGUAGE RankNTypes #-}
> {-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

> module Control.Quiver.Trans (
>   qRunErrorT,
>   qRunExceptT,
>   qRunMaybeT,
>   qRunRWST, qRunLazyRWST, qRunStrictRWST,
>   qRunReaderT,
>   qRunStateT, qRunLazyStateT, qRunStrictStateT,
>   qRunWriterT, qRunLazyWriterT, qRunStrictWriterT,
> ) where

> import Control.Monad.Trans.Error
> import Control.Monad.Trans.Except
> import Control.Monad.Trans.Maybe
> import Control.Monad.Trans.RWS.Lazy as Lazy
> import Control.Monad.Trans.RWS.Strict as Strict
> import Control.Monad.Trans.Reader
> import Control.Monad.Trans.State.Lazy as Lazy
> import Control.Monad.Trans.State.Strict as Strict
> import Control.Monad.Trans.Writer.Lazy as Lazy
> import Control.Monad.Trans.Writer.Strict as Strict
> import Control.Quiver.Internal

> qRunErrorT :: Functor m => P a a' b b' (ErrorT e m) r -> P a a' b b' m (Either e r)
> qRunErrorT = loop
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunErrorT q)
>   loop (Produce y k q) = produce y (loop . k) (qRunErrorT q)
>   loop (Enclose f)     = enclose (either (deliver . Left) loop <$> runErrorT f)
>   loop (Deliver r)     = deliver (Right r)

> qRunExceptT :: Functor m => P a a' b b' (ExceptT e m) r -> P a a' b b' m (Either e r)
> qRunExceptT = loop
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunExceptT q)
>   loop (Produce y k q) = produce y (loop . k) (qRunExceptT q)
>   loop (Enclose f)     = enclose (either (deliver . Left) loop <$> runExceptT f)
>   loop (Deliver r)     = deliver (Right r)

> qRunMaybeT :: Functor m => P a a' b b' (MaybeT m) r -> P a a' b b' m (Maybe r)
> qRunMaybeT = loop
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunMaybeT q)
>   loop (Produce y k q) = produce y (loop . k) (qRunMaybeT q)
>   loop (Enclose f)     = enclose (maybe (deliver Nothing) loop <$> runMaybeT f)
>   loop (Deliver r)     = deliver (Just r)

> qRunRWST :: (Functor m, Monoid mw) => P a a' b b' (Lazy.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
> qRunRWST = qRunLazyRWST

> qRunLazyRWST :: (Functor m, Monoid mw) => P a a' b b' (Lazy.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
> qRunLazyRWST p mr ms = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunLazyRWST q mr ms)
>   loop (Produce y k q) = produce y (loop . k) (qRunLazyRWST q mr ms)
>   loop (Enclose f)     = enclose (run <$> Lazy.runRWST f mr ms)
>   loop (Deliver r)     = deliver (r, ms, mempty)
>   run ~(p', ms', mw)    = adj mw <$> qRunLazyRWST p' mr ms'
>   adj mw ~(r, ms', mw') = (r, ms', mappend mw mw')

> qRunStrictRWST :: (Functor m, Monoid mw) => P a a' b b' (Strict.RWST mr mw ms m) r -> mr -> ms -> P a a' b b' m (r, ms, mw)
> qRunStrictRWST p mr ms = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunStrictRWST q mr ms)
>   loop (Produce y k q) = produce y (loop . k) (qRunStrictRWST q mr ms)
>   loop (Enclose f)     = enclose (run <$> Strict.runRWST f mr ms)
>   loop (Deliver r)     = deliver (r, ms, mempty)
>   run (p', ms', mw)    = adj mw <$> qRunStrictRWST p' mr ms'
>   adj mw (r, ms', mw') = (r, ms', mappend mw mw')

> qRunReaderT :: Functor m => P a a' b b' (ReaderT mr m) r -> mr -> P a a' b b' m r
> qRunReaderT p mr = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunReaderT q mr)
>   loop (Produce y k q) = produce y (loop . k) (qRunReaderT q mr)
>   loop (Enclose f)     = enclose (loop <$> runReaderT f mr)
>   loop (Deliver r)     = deliver r

> qRunStateT :: Functor m => P a a' b b' (Lazy.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
> qRunStateT = qRunLazyStateT

> qRunLazyStateT :: Functor m => P a a' b b' (Lazy.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
> qRunLazyStateT p ms = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunLazyStateT q ms)
>   loop (Produce y k q) = produce y (loop . k) (qRunLazyStateT q ms)
>   loop (Enclose f)     = enclose (run <$> Lazy.runStateT f ms)
>   loop (Deliver r)     = deliver (r, ms)
>   run ~(p', ms')       = qRunLazyStateT p' ms'

> qRunStrictStateT :: Functor m => P a a' b b' (Strict.StateT ms m) r -> ms -> P a a' b b' m (r, ms)
> qRunStrictStateT p ms = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunStrictStateT q ms)
>   loop (Produce y k q) = produce y (loop . k) (qRunStrictStateT q ms)
>   loop (Enclose f)     = enclose (uncurry qRunStrictStateT <$> Strict.runStateT f ms)
>   loop (Deliver r)     = deliver (r, ms)

> qRunWriterT :: (Functor m, Monoid mw) => P a a' b b' (Lazy.WriterT mw m) r -> P a a' b b' m (r, mw)
> qRunWriterT = qRunLazyWriterT

> qRunLazyWriterT :: (Functor m, Monoid mw) => P a a' b b' (Lazy.WriterT mw m) r -> P a a' b b' m (r, mw)
> qRunLazyWriterT p = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunLazyWriterT q)
>   loop (Produce y k q) = produce y (loop . k) (qRunLazyWriterT q)
>   loop (Enclose f)     = enclose (run <$> Lazy.runWriterT f)
>   loop (Deliver r)     = deliver (r, mempty)
>   run ~(p', mw)        = adj mw <$> qRunLazyWriterT p'
>   adj mw ~(r, mw')     = (r, mappend mw mw')

> qRunStrictWriterT :: (Functor m, Monoid mw) => P a a' b b' (Strict.WriterT mw m) r -> P a a' b b' m (r, mw)
> qRunStrictWriterT p = loop p
>  where
>   loop (Consume x k q) = consume x (loop . k) (qRunStrictWriterT q)
>   loop (Produce y k q) = produce y (loop . k) (qRunStrictWriterT q)
>   loop (Enclose f)     = enclose (run <$> Strict.runWriterT f)
>   loop (Deliver r)     = deliver (r, mempty)
>   run (p', mw)         = adj mw <$> qRunStrictWriterT p'
>   adj mw (r, mw')      = (r, mappend mw mw')



