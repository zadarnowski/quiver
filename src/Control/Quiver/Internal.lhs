> {-# LANGUAGE RankNTypes, TupleSections #-}

> module Control.Quiver.Internal (
>   Q (..),
>   consume, produce, enclose, deliver,
>   decouple, evacuate
> ) where

> data Q a' a b' b f c =
>     Consume a' (a  -> Q a' a b' b f c) (forall x' x . Q x' x b' b f c)
>   | Produce b  (b' -> Q a' a b' b f c) (forall x' x . Q a' a x' x f c)
>   | Enclose (f (Q a' a b' b f c))
>   | Deliver c

> consume :: a' -> (a  -> Q a' a b' b f c) -> (forall x' x . Q x' x b' b f c) -> Q a' a b' b f c
> consume = Consume

> produce :: b  -> (b' -> Q a' a b' b f c) -> (forall x' x . Q a' a x' x f c) -> Q a' a b' b f c
> produce = Produce

> enclose :: f (Q a' a b' b f c) -> Q a' a b' b f c
> enclose = Enclose

> deliver :: c -> Q a' a b' b f c
> deliver = Deliver

> instance Functor f => Functor (Q a' a b' b f) where
>   fmap ff (Consume x k r) = Consume x (fmap ff . k) (fmap ff r)
>   fmap ff (Produce y k r) = Produce y (fmap ff . k) (fmap ff r)
>   fmap ff (Enclose f)     = Enclose (fmap (fmap ff) f)
>   fmap ff (Deliver z)     = Deliver (ff z)

> instance Applicative f => Applicative (Q a' a b' b f) where
>   pure = Deliver
>   (Consume x k r) <*> q = Consume x ((<*> q) . k) (r <*> decouple q)
>   (Produce y k r) <*> q = Produce y ((<*> q) . k) (r <*> evacuate q)
>   (Enclose f)     <*> q = Enclose (fmap (<*> q) f)
>   (Deliver z)     <*> q = fmap z q

> instance Monad f => Monad (Q a' a b' b f) where
>   (Consume x k r) >>= kk = Consume x ((>>= kk) . k) (r >>= decouple . kk)
>   (Produce y k r) >>= kk = Produce y ((>>= kk) . k) (r >>= evacuate . kk)
>   (Enclose f)     >>= kk = Enclose (fmap (>>= kk) f)
>   (Deliver z)     >>= kk = kk z

> decouple :: Functor f => Q a' a b' b f c -> forall x' x . Q x' x b' b f c
> decouple (Consume _ _ r) = r
> decouple (Produce y k r) = Produce y (decouple . k) (decouple r)
> decouple (Enclose f) = Enclose (fmap decouple f)
> decouple (Deliver z) = Deliver z

> evacuate :: Functor f => Q a' a b' b f c -> forall x' x . Q a' a x' x f c
> evacuate (Consume x k r) = Consume x (evacuate . k) (evacuate r)
> evacuate (Produce _ _ r) = r
> evacuate (Enclose f) = Enclose (fmap evacuate f)
> evacuate (Deliver z) = Deliver z
