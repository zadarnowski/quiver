> -- | Module:    Control.Quiver.SP
> -- Description: Simple stream processors
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

> {-# LANGUAGE PatternSynonyms, RankNTypes, ScopedTypeVariables, TupleSections #-}

> module Control.Quiver.SP (
>   module Control.Quiver,
>   SQ, SP, SPResult,
>   pattern SPComplete,
>   pattern SPFailed,
>   pattern SPIncomplete,
>   spfetch, spemit, (>:>), (>>!),
>   sppure, spid, spconcat,
>   spfold, spfold', spfoldl, spfoldl', spfoldr, spfoldr',
>   sptraverse, sptraverse_,
> ) where

> import Control.Quiver

> infixr 5 >:>
> infixl 1 >>!

> -- | A /simple processor step/ with a unit request type and an unspecified
> --   response type:

> type SQ a b f r = forall b' . P () a b b' f r

> -- | A /simple processor/ with a unit request type, an unspecified
> --   response type and a result type tailored towards reporting the
> --   terminating condition of an intermediate component in a composed
> --   “processor stack”.

> type SP a b f e = SQ a b f (SPResult e)

> -- | Simple processor result type.

> type SPResult e = Maybe (Maybe e)

> -- | Simple processor result value indicating successful processing of the entire input stream.

> pattern SPComplete = Just Nothing

> -- | Simple processor result value indicating unsuccessful processing of the input stream.

> pattern SPFailed e = Just (Just e)

> -- | Simple processor result value indicating premature termination of the consumer.

> pattern SPIncomplete = Nothing

> -- | @spfetch@ represents a singleton simple stream processor that
> --   sends the request value @x@ upstream and delivers the
> --   next input value received, or @Nothing@ if the upstream
> --   processor has been depleted.

> spfetch :: Functor f => SQ a b f (Maybe a)
> spfetch = fetch ()

> -- | @spemit y@ represents a singleton stream processor that
> --   produces a single output value @y@, delivering either
> --   'SPComplete' if @y@ was consumed by the downstream processor,
> --   or 'SPIncomplete' otherwise.

> spemit :: b -> P a a' b b' f (SPResult e)
> spemit y = produce y (const $ deliver SPComplete) (deliver SPIncomplete)

> -- | @y >:> p@ represents a singleton stream processor that
> --   produces a single output value @y@ and continues with
> --   the processor 'p', deliverying 'SPIncomplete' if 'y' could
> --   not be consumed by the downstream processor.

> (>:>) :: b -> P a a' b b' f (SPResult e) -> P a a' b b' f (SPResult e)
> y >:> p = produce y (const p) (deliver SPIncomplete)

> -- | @p >>! k@ is equivalent to @p@, with any failures in @p@
> --   supplied to the continuation processor @k@. Note that
> --   @k@ is not executed if @p@ completes successfully with
> --   'SPComplete' or is interrupted by the downstream processor,
> --   deliverying 'SPIncomplete'.

> (>>!) :: Monad f => P a a' b b' f (SPResult e) -> (e -> P a a' b b' f (SPResult e')) -> P a a' b b' f (SPResult e')
> p >>! k = p >>= maybe (deliver SPIncomplete) (maybe (deliver SPComplete) k)

> -- | @sppure f@ produces an infinite consumer/producer that
> --   uses a pure function @f@ to convert every input value into
> --   an output; equivalent to @qpure id f (const ())@.

> sppure :: (a -> b) -> SP a b f e
> sppure f = cloop
>  where
>   cloop = consume () ploop (deliver SPComplete)
>   ploop x = produce (f x) (const cloop) (deliver SPIncomplete)

> -- | A simple identity processor, equivalent to 'sppure id'.

> spid :: SP a a f e
> spid = cloop
>  where
>   cloop = consume () ploop (deliver SPComplete)
>   ploop x = produce x (const cloop) (deliver SPIncomplete)

> -- | A simple list flattening processor requests.

> spconcat :: SP [a] a f e
> spconcat = cloop
>  where
>   cloop = consume () ploop (deliver SPComplete)
>   ploop (x:xs) = produce x (const $ ploop xs) (deliver SPIncomplete)
>   ploop [] = cloop

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using 'mappend'.

> spfold :: Monoid a => SQ a x f a
> spfold = cloop mempty
>  where
>   cloop r = consume () (cloop . mappend r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of 'mappend'.

> spfold' :: Monoid a => SQ a x f a
> spfold' = cloop mempty
>  where
>   cloop r = r `seq` consume () (cloop . mappend r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using the supplied left-associative function
> --   and initial value.

> spfoldl :: (b -> a -> b) -> b -> SQ a x f b
> spfoldl f = cloop
>  where
>   cloop r = consume () (cloop . f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of the supplied
> --   left-associative function and initial value.

> spfoldl' :: (b -> a -> b) -> b -> SQ a x f b
> spfoldl' f = cloop
>  where
>   cloop r = r `seq` consume () (cloop . f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using the supplied right-associative function
> --   and initial value.

> spfoldr :: (a -> b -> b) -> b -> SQ a x f b
> spfoldr f = cloop
>  where
>   cloop r = consume () (cloop . flip f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of the supplied
> --   right-associative function and initial value.

> spfoldr' :: (a -> b -> b) -> b -> SQ a x f b
> spfoldr' f = cloop
>  where
>   cloop r = r `seq` consume () (cloop . flip f r) (deliver r)

> -- | A processor that applies a monadic function to every input
> --   element and emits the resulting value.

> sptraverse :: Monad m => (a -> m b) -> SP a b m e
> sptraverse k = loop
>  where
>   loop = consume () loop' (deliver SPComplete)
>   loop' x = qlift (k x) >>= (>:> loop)

> -- | A processor that consumes every input elemnet using a monadic function.

> sptraverse_ :: Monad m => (a -> m ()) -> SP a b m e
> sptraverse_ k = loop
>  where
>   loop = consume () loop' (deliver SPComplete)
>   loop' x = qlift (k x) >> loop
