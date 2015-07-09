> -- | Module:    Control.Quiver.Internal
> -- Description: Common definitions
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module provides a host of common definitions,
> -- including the main Quiver /processor/ type @P@,
> -- that are reexported by other Quiver modules as
> -- required.
> --
> -- This is the only module in the Quiver library that
> -- exposes the actual four constructors of the stream
> -- processor type @P@, allowing for definition of low
> -- level stream processor transformations, such as
> -- conversions between @P@ and other stream processing
> -- libraries.
> --
> -- As a matter of style, Quiver users should strive to
> -- avoid explicit pattern matching on the @P@ type and
> -- rely instead on the various high level combinators
> -- exported elsewhere, in order to improve chances of
> -- successful deforestation by the various Quiver
> -- rewrite rules.

> {-# LANGUAGE RankNTypes, TupleSections #-}

> module Control.Quiver.Internal (
>   P (..), SP, Producer, Consumer, Effect,
>   consume, produce, enclose, deliver,
>   decouple, deplete,
> ) where


  Data Types
  ==========

> -- | The main Quiver /stream processor/ type @P a' a b b' f r@,
> --   representing a producer/consumer structure with /bidirectional/,
> --   /bounded/ communication on both the upstream (consumer) and
> --   downstream (producer) channel. The six type parameters have
> --   the following intuitive meaning:
> --
> --   * @a'@ is the type of a /request/ values sent by the stream
> --     processor to its upstream partner in order to receive the
> --     next element of the input stream.
> --
> --   * @a@ is the type of the actual information being consumed
> --     by this stream processor (i.e., elements of its input stream.)
> --
> --   * @b@ is the type of the actual information being produced
> --     by this stream processor (i.e., elements of its output stream.)
> --
> --   * @b'@ is the type of the /response/ values received from
> --     the downstream partner for each elemnet of the output
> --     stream produced by this stream processor.
> --
> --   * @f@ is the type of the stream processor's /base functor/;
> --     usually this is a monad used for stateful stream processing,
> --     exception handling and/or real-world interaction.
> --
> --   * @r@ is the stream processor's /delivery type/, used for
> --     monadic stream processor definition.
> --
> --   Every stream processor is a functor over its delivery type.
> --   However, if the base functor @f@ meets the additional requirements
> --   of 'Applicative' or 'Monad', so will the stream processor itself.
> --   Note that, unlike most other stream processing libraries, @f@
> --   is not required to be a monad in most applications, although
> --   only time will tell whether this generalisation has useful
> --   applications in the real world.

> data P a' a b b' f r =

>   -- | @Consume x k q@ represents a /consumer step/, in which
>   --   the request @x@ is sent upstream and the returned input
>   --   value is supplied to the /continuation processor/ @k@,
>   --   or, if the upstream partner has been /depleted/ (i.e.,
>   --   delivered its ultimate result, hence reaching the end
>   --   of processing), to the /decoupled continuation/ @q@.

>   Consume a' (a -> P a' a b b' f r) (Producer b b' f r) |

>   -- | @Produce y k q@ represent a /producer step/, in which
>   --   the output value @y@ is sent downstream, and the returned
>   --   acknowledgement is supplied to the /continuation processor/
>   --   @k@, or, if the downstream partner has been /decoupled/
>   --   (i.e., delivered its ultimate result, hence reaching the end
>   --   of processing), to the /depleted continuation/ @q@.

>   Produce b  (b' -> P a' a b b' f r) (Consumer a' a f r) |

>   -- | @Enclose@ allows for selective application of the base
>   --   functor @f@ the the remainder of the computation.

>   Enclose (f (P a' a b b' f r)) |

>   -- | @Deliver r@ completes processing of information, delivering
>   --   its ultimate result @r@.

>   Deliver r

> -- | A /simple processor/ with a unit request type and an unspecified
> --   response type:

> type SP a b f r = forall b' . P () a b b' f r

> -- | A Quiver /producer/, represented by a stream processor
> --   with unspecified input types.

> type Producer b b' f r = forall a' a . P a' a b b' f r

> -- | A Quiver /consumer/, represented by a stream processor
> --   with unspecified output types.

> type Consumer a' a f r = forall b b' . P a' a b b' f r

> -- | A Quiver /effect/, represented by a stream processor
> --   with unspecified input and output types.

> type Effect f r = forall a' a b b' . P a' a b b' f r


  Instances
  =========

> instance Functor f => Functor (P a' a b b' f) where
>   fmap ff (Consume x k q) = Consume x (fmap ff . k) (fmap ff q)
>   fmap ff (Produce y k q) = Produce y (fmap ff . k) (fmap ff q)
>   fmap ff (Enclose f)     = Enclose (fmap (fmap ff) f)
>   fmap ff (Deliver r)     = Deliver (ff r)
>   r <$ (Consume x k q) = Consume x ((r <$) . k) (r <$ q)
>   r <$ (Produce y k q) = Produce y ((r <$) . k) (r <$ q)
>   r <$ (Enclose f)     = Enclose (fmap (r <$) f)
>   r <$ (Deliver _)     = Deliver r

> instance Applicative f => Applicative (P a' a b b' f) where
>   pure = Deliver
>   (Consume x k q) <*> p = Consume x ((<*> p) . k) (q <*> decouple p)
>   (Produce y k q) <*> p = Produce y ((<*> p) . k) (q <*> deplete p)
>   (Enclose f)     <*> p = Enclose (fmap (<*> p) f)
>   (Deliver r)     <*> p = fmap r p

> instance Monad f => Monad (P a' a b b' f) where
>   (Consume x k q) >>= kk = Consume x ((>>= kk) . k) (q >>= decouple . kk)
>   (Produce y k q) >>= kk = Produce y ((>>= kk) . k) (q >>= deplete . kk)
>   (Enclose f)     >>= kk = Enclose (fmap (>>= kk) f)
>   (Deliver r)     >>= kk = kk r


  Primitive Combinators
  =====================

> -- | @consume x k q@ represents a /consumer step/, in which
> --   the request @x@ is sent upstream and the returned input
> --   value is supplied to the /continuation processor/ @k@,
> --   or, if the upstream partner has been /depleted/ (i.e.,
> --   delivered its ultimate result, hence reaching the end
> --   of processing), to the /decoupled continuation/ @q@.

> consume :: a' -> (a -> P a' a b b' f r) -> Producer b b' f r -> P a' a b b' f r
> consume = Consume

> -- | @produce y k q@ represent a /producer step/, in which
> --   the output value @y@ is sent downstream, and the returned
> --   acknowledgement is supplied to the /continuation processor/
> --   @k@, or, if the downstream partner has been /decoupled/
> --   (i.e., delivered its ultimate result, hence reaching the end
> --   of processing), to the /depleted continuation/ @q@.

> produce :: b  -> (b' -> P a' a b b' f r) -> Consumer a' a f r -> P a' a b b' f r
> produce = Produce

> -- | @enclose@ allows for selective application of the base
> --   functor @f@ the the remainder of the computation.

> enclose :: f (P a' a b b' f r) -> P a' a b b' f r
> enclose = Enclose

> -- | @deliver r@ completes processing of information, delivering
> --   its ultimate result @r@.

> deliver :: r -> P a' a b b' f r
> deliver = Deliver


  Utilities
  =========

> -- | @decouple p@ /decouples/ the stream processor @p@, by replacing
> --   the first consumer step in @p@ with that step's decoupled contination,
> --   effectively converting @p@ into a producer processor that no longer
> --   expects to receive any input.

> decouple :: Functor f => P a' a b b' f r -> Producer b b' f r
> decouple (Consume _ _ q) = q
> decouple (Produce y k q) = Produce y (decouple . k) (decouple q)
> decouple (Enclose f) = Enclose (fmap decouple f)
> decouple (Deliver r) = Deliver r

> -- | @deplete p@ /depletes/ the stream processor @p@, by replacing
> --   the first producer step in @p@ with that step's depleted contination,
> --   effectively converting @p@ into a consumer processor that will never
> --   produce any more output.

> deplete :: Functor f => P a' a b b' f r -> Consumer a' a f r
> deplete (Consume x k q) = Consume x (deplete . k) (deplete q)
> deplete (Produce _ _ q) = q
> deplete (Enclose f) = Enclose (fmap deplete f)
> deplete (Deliver r) = Deliver r

