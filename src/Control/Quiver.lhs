> {-# LANGUAGE RankNTypes, TupleSections #-}

> module Control.Quiver (
>   Q,
>   consume, produce, enclose, deliver,
>   decouple, evacuate,
>   fetch, fetch',
>   emit, emit', emit_,
>   liftQ,
>   (>>->), (>->>),
> ) where

> import Control.Quiver.Internal

> infixl 0 >>->, >->>

> fetch :: a' -> Q a' a b' b f (Maybe a)
> fetch x = Consume x (Deliver . Just) (Deliver Nothing)

> fetch' :: a' -> (forall x' x . Q x' x b' b f a) -> Q a' a b' b f a
> fetch' x r = Consume x Deliver r

> emit :: b -> Q a' a b' b f (Maybe b')
> emit y = Produce y (Deliver . Just) (Deliver Nothing)

> emit' :: b -> (forall x' x . Q a' a x' x f b') -> Q a' a b' b f b'
> emit' y r = Produce y Deliver r

> emit_ :: b -> Q a' a b' b f ()
> emit_ y = Produce y (Deliver . const ()) (Deliver ())

> liftQ :: Functor f => f c -> Q a' a b' b f c
> liftQ = Enclose . fmap Deliver

> (>>->) :: Functor f => Q a' a t' t f c1 -> Q t' t b' b f c2 -> Q a' a b' b f (c1, c2)
> (Consume x1 k1 r1) >>-> q2 = Consume x1 ((>>-> q2) . k1) (r1 >>-> q2)
> (Produce y1 k1 r1) >>-> q2 = loop q2
>  where
>   loop  (Consume x2 k2  _) = k1 x2 >>-> k2 y1
>   loop  (Produce y2 k2 r2) = Produce y2 (loop . k2) (loop' r2)
>   loop  (Enclose f2)       = Enclose (fmap loop f2)
>   loop  (Deliver z2)       = fmap (, z2) r1
>   loop' (Consume x2 k2  _) = k1 x2 >>-> k2 y1
>   loop' (Produce  _  _ r2) = loop' r2
>   loop' (Enclose f2)       = Enclose (fmap loop' f2)
>   loop' (Deliver z2)       = fmap (, z2) r1
> (Enclose f1) >>-> q2 = Enclose (fmap (>>-> q2) f1)
> (Deliver z1) >>-> q2 = fmap (z1 ,) (decouple q2)

> (>->>) :: Functor f => Q a' a t' t f c1 -> Q t' t b' b f c2 -> Q a' a b' b f (c1, c2)
> q1 >->> (Consume x2 k2 r2) = loop q1
>  where
>   loop  (Consume x1 k1 r1) = Consume x1 (loop . k1) (loop' r1)
>   loop  (Produce y1 k1  _) = k1 x2 >->> k2 y1
>   loop  (Enclose f1)       = Enclose (fmap loop f1)
>   loop  (Deliver z1)       = fmap (z1 ,) r2
>   loop' (Consume  _  _ t1) = loop' t1
>   loop' (Produce y1 k1  _) = k1 x2 >->> k2 y1
>   loop' (Enclose f1)       = Enclose (fmap loop' f1)
>   loop' (Deliver z1)       = fmap (z1 ,) r2
> q1 >->> (Produce y2 k2 r2) = Produce y2 ((q1 >->>) . k2) (q1 >->> r2)
> q1 >->> (Enclose f2)       = Enclose (fmap (q1 >->>) f2)
> q1 >->> (Deliver z2)       = fmap (, z2) (evacuate q1)
