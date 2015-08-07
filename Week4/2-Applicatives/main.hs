-- Task 1
data BTree a = Node a (BTree a) (BTree a) | Leaf a | Nil
  deriving (Show)

instance Functor BTree where
--  fmap :: (a -> b) -> fa -> fb
    fmap f Nil = Nil
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node a b c) = Node a $ f b $ f c

-- Task 2
data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

--instance Functor ((,) a) where
--    fmap  f (a,b) = (a,f b)
