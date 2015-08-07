import Data.Monoid
import Control.Applicative

--task 1
--instance Monad Maybe where
--    return = Just
--    Just x >>= f = f x
--    Nothing >>= _ = Nothing

data List a = Cons a (List a) | Nil
    deriving (Show)

appendList :: List a -> List a -> List a
appendList (Cons x xs) ys = Cons x (appendList xs ys)
appendList Nil ys = ys

instance Monad List where
    return x = Cons x Nil
    Nil >>= f = Nil
    Cons x xs >>= f = f x `appendList` (xs >>= f)

--instance Monad ((->) a) where
--    return x = const x
--    (>>=) :: (a -> b) -> (b -> (a -> d)) -> (a -> d)
--    fv(>>=) g = \a -> g (f a) a

--instance Monad IO where
--    return a = IO a
--    IO a (>>=) f = f a

--task 2
join :: Monad m => m (m a) -> m a
join m = m >>= id-- \a -> a

--task 3
sequenceM :: Monad m => [m a] -> m [a]
sequenceM (x:xs) = x >>= \a ->
    (sequenceM xs >>= \as ->return $ a : as)
sequenceM [] = return []

--task 4
replicateM :: Monad m => Int -> m a -> m [a]
replicateM x m = sequenceM (helpRep 0) where
    helpRep count
        | count == x = [m]
        | otherwise = m : helpRep (count + 1)

--task 5
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f (x:xs) = f x >>= \b -> if b then filterM f xs >>= \as -> return (x : as) else filterM f xs
filterM _ [] = return []

--task 6
mapMM :: Monad m => (a -> m b) -> [a] -> m [b]
mapMM f (x:xs) = f x >>= \c -> (mapMM f xs >>= \xc -> return (c : xc))
mapMM _ [] = return []

--task 7
foldM :: Monad m => (a -> b ->  m a) -> a -> [b] ->  m a
foldM f y (x:xs) = f y x >>= \c -> foldM f c xs 
foldM _ y [] = return y

--Day2
--Task1
newtype Writer w a = Writer { runWriter :: (a, w) }
    deriving(Show)

execWriter :: Writer w a -> w
execWriter = snd . runWriter

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f (Writer (a, w)) = Writer $ f (a, w)

tell :: w -> Writer w ()
tell w = Writer ((), w)

--Task 2
instance Functor (Writer w) where
    --fmap :: (a -> b) -> fa -> fb
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (Writer (f, w)) <*> (Writer (a, w')) = Writer (f a, w `mappend` w')

instance Monoid w => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (a, w) >>= f = let Writer (b, w') = f a in Writer (b, w `mappend` w')

--Task 3
