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
>   SQ, SP, SProducer, SConsumer, SEffect, SPResult,
>   pattern SPComplete,
>   pattern SPFailed,
>   pattern SPIncomplete,
>   spcomplete, spfailed, spincomplete,
>   spconsume,
>   spfetch, spemit, (>:>), (>>?), (>>!),
>   sppure, spid, spconcat, spfilter,
>   spfold, spfold', spfoldl, spfoldl', spfoldr, spfoldr',
>   sptraverse, sptraverse_,
>   spevery,
>   spforever,
>   spuntil, spwhile, spWhileJust,
>   sprun,
> ) where

> import Control.Quiver
> import Control.Quiver.Internal

> infixr 5 >:>
> infixl 1 >>?, >>!

> -- | A /simple processor step/ with a unit request type and an unspecified
> --   response type:

> type SQ a b f r = forall b' . P () a b b' f r

> -- | A /simple processor/ with a unit request type, an unspecified
> --   response type and a result type tailored towards reporting the
> --   terminating condition of an intermediate component in a composed
> --   “processor stack”.

> type SP a b f e = SQ a b f (SPResult e)

> -- | A producer version of a simple processor.

> type SProducer b f e = forall a . SP a b f e

> -- | A consumer version of a simple processor.

> type SConsumer a f e = forall b . SP a b f e

> -- | An effect version of a simple processor.

> type SEffect f e = forall a b . SP a b f e

> -- | Simple processor result type.

> type SPResult e = Maybe (Maybe e)

> -- | (@'Just' 'Nothing'@) Simple processor result value indicating successful processing of the entire input stream.

> pattern SPComplete = Just Nothing

> -- | (@'Just' ('Just' e)'@) Simple processor result value indicating unsuccessful processing of the input stream.

> pattern SPFailed e = Just (Just e)

> -- | ('Nothing') Simple processor result value indicating premature termination of the consumer.

> pattern SPIncomplete = Nothing

> -- | Delivers an 'SPComplete' result.

> spcomplete :: P a a' b b' f (SPResult e)
> spcomplete = deliver SPComplete

> -- | Delivers an 'SPFailed' result.

> spfailed :: e -> P a a' b b' f (SPResult e)
> spfailed = deliver . SPFailed

> -- | Delivers an 'SPIncomplete' result.

> spincomplete :: P a a' b b' f (SPResult e)
> spincomplete = deliver SPIncomplete

> -- | Consumes an single input value of a simple stream processor.

> spconsume :: (a' -> P () a' b b' f r) -> (Producer b b' f r) -> P () a' b b' f r
> spconsume = consume ()

> -- | @spfetch@ represents a singleton simple stream processor that
> --   delivers the next input value received, or @Nothing@ if the
> --   upstream processor has been depleted.

> spfetch :: Functor f => SQ a b f (Maybe a)
> spfetch = fetch ()

> -- | @spemit y@ represents a singleton stream processor that
> --   produces a single output value @y@, delivering either
> --   'SPComplete' if @y@ was consumed by the downstream processor,
> --   or 'SPIncomplete' otherwise.

> spemit :: b -> P a a' b b' f (SPResult e)
> spemit y = produce y (const spcomplete) spincomplete

> -- | @y >:> p@ represents a singleton stream processor that
> --   produces a single output value @y@ and continues with
> --   the processor 'p', deliverying 'SPIncomplete' if 'y' could
> --   not be consumed by the downstream processor.

> (>:>) :: b -> P a a' b b' f (SPResult e) -> P a a' b b' f (SPResult e)
> y >:> p = produce y (const p) spincomplete

> -- | @p >>? q@ continues processing of @p@ with @q@ but only
> --   if @p@ completes successsfully by delivering 'SPComplete',
> --   short-circuiting @q@ if @p@ fails with 'SPIncomplete' or
> --   'SPFailed'.

> (>>?) :: Monad f => P a a' b b' f (SPResult e) -> P a a' b b' f (SPResult e) -> P a a' b b' f (SPResult e)
> p >>? q = p >>= maybe spincomplete (maybe q spfailed)

> -- | @p >>! k@ is equivalent to @p@, with any failures in @p@
> --   supplied to the continuation processor @k@. Note that
> --   @k@ is not executed if @p@ completes successfully with
> --   'SPComplete' or is interrupted by the downstream processor,
> --   delivering 'SPIncomplete'.

> (>>!) :: Monad f => P a a' b b' f (SPResult e) -> (e -> P a a' b b' f (SPResult e')) -> P a a' b b' f (SPResult e')
> p >>! k = p >>= maybe spincomplete (maybe spcomplete k)

> -- | @sppure f@ produces an infinite consumer/producer that
> --   uses a pure function @f@ to convert every input value into
> --   an output; equivalent to @qpure id f (const ())@.

> sppure :: (a -> b) -> SP a b f e
> sppure f = cloop
>  where
>   cloop = consume () ploop spcomplete
>   ploop x = produce (f x) (const cloop) spincomplete

> -- | A simple identity processor, equivalent to 'sppure id'.

> spid :: SP a a f e
> spid = cloop
>  where
>   cloop = consume () ploop spcomplete
>   ploop x = produce x (const cloop) spincomplete

> -- | A simple list flattening processor requests.

> spconcat :: Foldable t => SP (t a) a f e
> spconcat = spconsume (foldr (>:>) spconcat) spcomplete

> -- | A simple processor that filters its input stream.

> spfilter :: (a -> Bool) -> SP a a f e
> spfilter f = loop
>  where
>   loop = spconsume loop' spcomplete
>   loop' x = if f x then x >:> loop else loop

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using 'mappend'.

> spfold :: Monoid a => SQ a x f a
> spfold = cloop mempty
>  where
>   cloop r = spconsume (cloop . mappend r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of 'mappend'.

> spfold' :: Monoid a => SQ a x f a
> spfold' = cloop mempty
>  where
>   cloop r = r `seq` spconsume (cloop . mappend r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using the supplied left-associative function
> --   and initial value.

> spfoldl :: (b -> a -> b) -> b -> SQ a x f b
> spfoldl f = cloop
>  where
>   cloop r = spconsume (cloop . f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of the supplied
> --   left-associative function and initial value.

> spfoldl' :: (b -> a -> b) -> b -> SQ a x f b
> spfoldl' f = cloop
>  where
>   cloop r = r `seq` spconsume (cloop . f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using the supplied right-associative function
> --   and initial value.

> spfoldr :: (a -> b -> b) -> b -> SQ a x f b
> spfoldr f = cloop
>  where
>   cloop r = spconsume (cloop . flip f r) (deliver r)

> -- | A processor that delivers the entire input of the stream folded
> --   into a single value using strict application of the supplied
> --   right-associative function and initial value.

> spfoldr' :: (a -> b -> b) -> b -> SQ a x f b
> spfoldr' f = cloop
>  where
>   cloop r = r `seq` spconsume (cloop . flip f r) (deliver r)

> -- | A processor that applies a monadic function to every input
> --   element and emits the resulting value.

> sptraverse :: Monad m => (a -> m b) -> SP a b m e
> sptraverse k = loop
>  where
>   loop = spconsume loop' spcomplete
>   loop' x = qlift (k x) >>= (>:> loop)

> -- | A processor that consumes every input elemnet using a monadic function.

> sptraverse_ :: Monad m => (a -> m ()) -> SConsumer a m e
> sptraverse_ k = loop
>  where
>   loop = spconsume loop' spcomplete
>   loop' x = qlift (k x) >> loop

> -- | Produces every element of a foldable structure.

> spevery :: Foldable t => t a -> SProducer a f e
> spevery = foldr (>:>) spcomplete

> -- | Produces infinite sequence of monadic results.

> spforever :: Functor f => f a -> SProducer a f e
> spforever f = loop
>  where
>   loop = enclose (fmap (>:> loop) f)

> -- | Interrupts processing on input that matches a specified predicate.

> spuntil :: (a -> Bool) -> SP a a f e
> spuntil f = loop
>  where
>   loop = spconsume loop' spcomplete
>   loop' x = if f x then spcomplete else x >:> loop

> -- | Interrupts processing on input that doesn't match a specified predicate.

> spwhile :: (a -> Bool) -> SP a a f e
> spwhile f = spuntil (not . f)

> -- | Interrupts processing on a first occurence of 'Nothing' in the input stream.

> spWhileJust :: SP (Maybe a) a f e
> spWhileJust = spconsume (maybe spcomplete (>:> spWhileJust)) spcomplete

> -- | Evaluates an 'SEffect', i.e., a simple processor that is both detached
> --   and depleted and hence neither consumes nor produces any input,
> --   returning its delivered value. The base functor must be a monad.

> sprun :: Monad f => forall a b . SQ a b f r -> f r
> sprun p = loop p
>  where
>   loop (Consume _ _ q) = loop q
>   loop (Produce _ _ q) = loop q
>   loop (Enclose f)     = f >>= loop
>   loop (Deliver r)     = return r

