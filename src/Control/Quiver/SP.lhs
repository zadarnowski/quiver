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
>   SP,
>   SPResult,
>   pattern SPIncomplete,
>   pattern SPComplete,
>   pattern SPFailed,
>   spfetch,
>   sppure, spid, spconcat,
>   spfold, spfoldl, spfoldr,
> ) where

> import Control.Quiver

> -- | A /simple processor/ with a unit request type and an unspecified
> --   response type:

> type SP a b f r = forall b' . P () a b b' f r

> -- | Simple processor result type.

> type SPResult e = Maybe (Maybe e)

> -- | Simple processor result value indicating premature termination of the consumer.

> pattern SPIncomplete = Nothing

> -- | Simple processor result value indicating successful processing of the entire input stream.

> pattern SPComplete = Just Nothing

> -- | Simple processor result value indicating unsuccessful processing of the input stream.

> pattern SPFailed e = Just (Just e)

> -- | @spfetch@ represents a singleton simple stream processor that
> --   sends the request value @x@ upstream and delivers the
> --   next input value received, or @Nothing@ if the upstream
> --   processor has been depleted.

> spfetch :: Functor f => SP a b f (Maybe a)
> spfetch = fetch ()

> -- | @sppure f@ produces an infinite consumer/producer that
> --   uses a pure function @f@ to convert every input value into
> --   an output; equivalent to @qpure id f (const ())@.

> sppure :: (a -> b) -> SP a b f ()
> sppure f = cloop
>  where
>   cloop = consume () ploop (deliver ())
>   ploop x = produce (f x) (const cloop) (deliver ())

> -- | A simple identity processor, equivalent to 'sppure id'.

> spid :: SP a a f ()
> spid = cloop
>  where
>   cloop = consume () ploop (deliver ())
>   ploop x = produce x (const cloop) (deliver ())

> -- | A simple list flattening processor requests.

> spconcat :: SP [a] a f [a]
> spconcat = cloop
>  where
>   cloop = consume () ploop (deliver [])
>   ploop (x:xs) = produce x (const $ ploop xs) (deliver xs)
>   ploop [] = cloop

> -- | A processor that folds an entire stream into a single value.

> spfold :: (Functor f, Monoid a) => SP a a f ()
> spfold = cloop mempty
>  where
>   cloop r = consume () (cloop . mappend r) (emit_ r)

> -- | A processor that folds an entire stream into a single value.

> spfoldl :: (b -> a -> b) -> b -> SP a b f ()
> spfoldl f = cloop
>  where
>   cloop r = consume () (cloop . f r) (emit_ r)

> -- | A processor that folds an entire stream into a single value.

> spfoldr :: (a -> b -> b) -> b -> SP a b f ()
> spfoldr f = cloop
>  where
>   cloop r = consume () (cloop . flip f r) (emit_ r)

