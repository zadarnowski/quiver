> -- | Module:    Control.Quiver
> -- Description: Core Quiver definitions
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module provides the core types and combinators
> -- of the Quiver stream processing library.

> {-# LANGUAGE RankNTypes, TupleSections #-}

> module Control.Quiver (
>   -- Imported from @Control.Quiver.Internal@:
>   P, Consumer, Producer, Effect,
>   consume, produce, enclose, deliver,
>   decouple, deplete,
>   -- Defined below:
>   fetch, fetch_,
>   emit, emit_,
>   qlift, qhoist, qembed,
>   qpure, qid, qconcat, qtraverse,
>   runEffect,
>   (>>->), (>->>), (+>>->), (>>->+), (+>->>), (>->>+), (>&>),
>   qcompose,
> ) where

> import Control.Quiver.Internal

> infixl 1 >>->, >->>, +>>->, >>->+, +>->>, >->>+, >&>

> -- | @fetch x@ represents a singleton stream processor that
> --   sends the request value @x@ upstream and delivers the
> --   next input value received, or @Nothing@ if the upstream
> --   processor has been depleted.

> fetch :: Functor f => a -> P a a' b b' f (Maybe a')
> fetch x = consume x (deliver . Just) (deliver Nothing)

> -- | @fetch_ x@ represents a singleton stream processor that
> --   sends the request value @x@ upstream, discarding any
> --   input received, for symmetry with @emit_@.

> fetch_ :: a -> P a a' b b' f ()
> fetch_ x = consume x (deliver . const ()) (deliver ())

> -- | @emit y@ represents a singleton stream processor that
> --   produces a single output value @y@ and delivers the
> --   response received from the downstream processor, or
> --   @Nothing@ if the downstream processor has been decoupled.

> emit :: b -> P a a' b b' f (Maybe b')
> emit y = produce y (deliver . Just) (deliver Nothing)

> -- | @emit_ y@ represents a singleton stream processor that
> --   produces a single output value @y@, ignoring any response
> --   received from the downstream processor.

> emit_ :: b -> P a a' b b' f ()
> emit_ y = produce y (deliver . const ()) (deliver ())

> -- | @qpure g f z@ produces an infinite consumer/producer that
> --   uses a pure function @f@ to convert every input value into
> --   an output, and @g@ to convert each downstream response value
> --   into an upstream request; the initial request is obtained
> --   by applying @g@ to the initial response value @z@.

> qpure :: (b' -> a) -> (a' -> b) -> b' -> P a a' b b' f (Either a b)
> qpure g f = cloop
>  where
>   cloop y = let y' = g y in consume y' ploop (deliver (Left y'))
>   ploop x = let x' = f x in produce x' cloop (deliver (Right x'))

> -- | A pull-based identity processor, equivalent to 'qpure id id'.

> qid :: b -> P b a a b f ()
> qid = cloop
>  where
>   cloop z = consume z ploop (deliver ())
>   ploop x = produce x cloop (deliver ())

> -- | A pull-based list flattening processor, delivering the list
> --   of inputs that could not be produced and a list of responses
> --   that could not be consumed.

> qconcat :: [b] -> P [b] [a] a b f ([a], [b])
> qconcat = cloop
>  where
>   cloop ys = consume ys (ploop []) (deliver ([], []))
>   ploop ys (x:xs) = produce x (\y -> ploop (y:ys) xs) (deliver (xs, reverse ys))
>   ploop ys [] = cloop (reverse ys)

> -- | @qtraverse g f z@ produces an infinite consumer/producer that
> --   uses a functor @f@ to convert every input value into
> --   an output, and @g@ to convert each downstream response value
> --   into an upstream request; the initial request is obtained
> --   by applying @g@ to the initial response value @z@.

> qtraverse :: Functor f => (b' -> f a) -> (a' -> f b) -> b' -> P a a' b b' f (Either a b)
> qtraverse g f = cloop
>  where
>   cloop y = enclose (fmap (\y' -> consume y' ploop (deliver (Left y'))) (g y))
>   ploop x = enclose (fmap (\x' -> produce x' cloop (deliver (Right x'))) (f x))

> -- | Evaluates an /effect/, i.e., a processor that is both detached
> --   and depleted and hence neither consumes nor produces any input,
> --   returning its delivered value. The base functor must be a monad.

> runEffect :: Monad f => Effect f r -> f r
> runEffect p = loop p
>  where
>   loop (Consume _ _ q) = loop q
>   loop (Produce _ _ q) = loop q
>   loop (Enclose f)     = f >>= loop
>   loop (Deliver r)     = return r

> -- | The @>>->@ represents a push-based composition of stream processors.
> --   @p1 >>-> p2@ represents a stream processor that forwards the output
> --   of @p1@ to @p2@, delivering the result of both processors.
> --   The new processor is /driven/ by @p2@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p2@ will be
> --   observed before those of @p1@.

> (>>->) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (r1, r2)
> (Consume x1 k1 q1) >>-> p2 = consume x1 ((>>-> p2) . k1) (q1 >>-> p2)
> (Produce y1 k1 q1) >>-> p2 = loop p2
>  where
>   loop (Consume x2 k2  _) = k1 x2 >>-> k2 y1
>   loop (Produce y2 k2 q2) = produce y2 (loop . k2) (deplete $ loop q2)
>   loop (Enclose f2)       = enclose (fmap loop f2)
>   loop (Deliver r2)       = fmap (, r2) q1
> (Enclose f1) >>-> p2 = enclose (fmap (>>-> p2) f1)
> (Deliver r1) >>-> p2 = fmap (r1 ,) (decouple p2)

> -- | The @+>>->@ represents a pull-based composition of stream processors
> --   that is partial on the left (supply) side, so that @p1 +>>-> p2@
> --   represents a stream processor that forwards the output of @p1@ to @p2@,
> --   delivering the result of @p2@ and the remainder (unconsumed portion)
> --   of @p1@. The new processor is /driven/ by @p1@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p1@ will be observed
> --   before those of @p2@.

> (+>>->) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (P a a' b b' f r1, r2)
> (Consume x1 k1 q1) +>>-> p2 = consume x1 ((+>>-> p2) . k1) (decouple $ q1 +>>-> p2)
> (Produce y1 k1 q1) +>>-> p2 = loop p2
>  where
>   loop (Consume x2 k2  _) = k1 x2 +>>-> k2 y1
>   loop (Produce y2 k2 q2) = produce y2 (loop . k2) (deplete $ loop q2)
>   loop (Enclose f2)       = enclose (fmap loop f2)
>   loop (Deliver r2)       = deliver (q1, r2)
> (Enclose f1) +>>-> p2 = enclose (fmap (+>>-> p2) f1)
> p1 +>>-> p2 = fmap (p1 ,) (decouple p2)

> -- | The @>>->+@ represents a pull-based composition of stream processors
> --   that is partial on the right (demand) side, so that @p1 >>->+ p2@
> --   represents a stream processor that forwards the output of @p1@ to @p2@,
> --   delivering the result of @p1@ and the remainder (unproduced portion)
> --   of @p2@. The new processor is /driven/ by @p1@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p1@ will be observed
> --   before those of @p2@.

> (>>->+) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (r1, P b' b c c' f r2)
> (Consume x1 k1 q1) >>->+ p2 = consume x1 ((>>->+ p2) . k1) (q1 >>->+ p2)
> (Produce y1 k1 q1) >>->+ p2 = loop p2
>  where
>   loop (Consume x2 k2  _) = k1 x2 >>->+ k2 y1
>   loop (Produce y2 k2 q2) = produce y2 (loop . k2) (deplete $ loop q2)
>   loop (Enclose f2)       = enclose (fmap loop f2)
>   loop p2'                = fmap (, p2') q1
> (Enclose f1) >>->+ p2 = enclose (fmap (>>->+ p2) f1)
> (Deliver r1) >>->+ p2 = deliver (r1, p2)

> -- | The @>->>@ represents a pull-based composition of stream processors.
> --   @p1 >->> p2@ represents a stream processor that forwards the output
> --   of @p1@ to @p2@, delivering the result of both processors.
> --   The new processor is /driven/ by @p2@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p2@ will be
> --   observed before those of @p1@.

> (>->>) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (r1, r2)
> p1 >->> (Consume x2 k2 q2) = loop p1
>  where
>   loop (Consume x1 k1 q1) = consume x1 (loop . k1) (decouple $ loop q1)
>   loop (Produce y1 k1  _) = k1 x2 >->> k2 y1
>   loop (Enclose f1)       = enclose (fmap loop f1)
>   loop (Deliver r1)       = fmap (r1 ,) q2
> p1 >->> (Produce y2 k2 q2) = produce y2 ((p1 >->>) . k2) (p1 >->> q2)
> p1 >->> (Enclose f2)       = enclose (fmap (p1 >->>) f2)
> p1 >->> (Deliver r2)       = fmap (, r2) (deplete p1)

> -- | The @+>->>@ represents a pull-based composition of stream processors.
> --   that is partial on the left (supply) side, so that @p1 +>->> p2@
> --   represents a stream processor that forwards the output of @p1@ to @p2@,
> --   delivering the result of @p2@ and the remainder (unconsumed portion)
> --   of @p1@. The new processor is /driven/ by @p2@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p2@ will be observed
> --   before those of @p1@.

> (+>->>) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (P a a' b b' f r1, r2)
> p1 +>->> (Consume x2 k2 q2) = loop p1
>  where
>   loop (Consume x1 k1 q1) = consume x1 (loop . k1) (decouple $ loop q1)
>   loop (Produce y1 k1  _) = k1 x2 +>->> k2 y1
>   loop (Enclose f1)       = enclose (fmap loop f1)
>   loop p1'                = fmap (p1' ,) q2
> p1 +>->> (Produce y2 k2 q2) = produce y2 ((p1 +>->>) . k2) (p1 +>->> q2)
> p1 +>->> (Enclose f2)       = enclose (fmap (p1 +>->>) f2)
> p1 +>->> (Deliver r2)       = deliver (p1, r2)

> -- | The @>>->+@ represents a pull-based composition of stream processors
> --   that is partial on the right (demand) side, so that @p1 >->>+ p2@
> --   represents a stream processor that forwards the output of @p1@ to @p2@,
> --   delivering the result of @p1@ and the remainder (unproduced portion)
> --   of @p2@. The new processor is /driven/ by @p2@, so, if the base functor
> --   represents a non-commutative monad, any effects of @p2@ will be observed
> --   before those of @p1@.

> (>->>+) :: Functor f => P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f (r1, P b' b c c' f r2)
> p1 >->>+ (Consume x2 k2 q2) = loop p1
>  where
>   loop (Consume x1 k1 q1) = consume x1 (loop . k1) (decouple $ loop q1)
>   loop (Produce y1 k1  _) = k1 x2 >->>+ k2 y1
>   loop (Enclose f1)       = enclose (fmap loop f1)
>   loop (Deliver r1)       = deliver (r1, q2)
> p1 >->>+ (Produce y2 k2 q2) = produce y2 ((p1 >->>+) . k2) (deplete $ p1 >->>+ q2)
> p1 >->>+ (Enclose f2)       = enclose (fmap (p1 >->>+) f2)
> p1 >->>+ p2                 = fmap (, p2) (deplete p1)

> -- | An infix version of @flip fmap@ with the same precedence and associativity
> --   as the stream processor composition operators '>->>' and '>>->', indended
> --   for idiomatic processing of composition deliverables using expressions
> --   such as @p >->> q >&> fst@.

> (>&>) :: Functor f => P a a' b b' f r -> (r -> r') -> P a a' b b' f r'
> (>&>) = flip fmap

> -- | The @qcompose f p q@ is precisely equivalent to @p >->> q >&> uncurry f@,
> --   but faster. A rewrite rule is included to replace applications of
> --   '>->>' followed by '>&>' into 'qcompose'.

> qcompose :: Functor f => (r1 -> r2 -> r) -> P a a' b b' f r1 -> P b' b c c' f r2 -> P a a' c c' f r
> qcompose ff p1 (Consume x2 k2 q2) = loop p1
>  where
>   loop  (Consume x1 k1 q1) = consume x1 (loop . k1) (loop' q1)
>   loop  (Produce y1 k1  _) = qcompose ff (k1 x2) (k2 y1)
>   loop  (Enclose f1)       = enclose (fmap loop f1)
>   loop  (Deliver r1)       = fmap (ff r1) q2
>   loop' (Consume  _  _ q1) = loop' q1
>   loop' (Produce y1 k1  _) = qcompose ff (k1 x2) (k2 y1)
>   loop' (Enclose f1)       = enclose (fmap loop' f1)
>   loop' (Deliver r1)       = fmap (ff r1) q2
> qcompose ff p1 (Produce y2 k2 q2) = produce y2 ((qcompose ff p1) . k2) (qcompose ff p1 q2)
> qcompose ff p1 (Enclose f2)       = enclose (fmap (qcompose ff p1) f2)
> qcompose ff p1 (Deliver r2)       = fmap (flip ff r2) (deplete p1)

> {-# RULES "qcompose/fmap" forall p q f . fmap f (p >->> q) = qcompose (curry f) p q #-}
