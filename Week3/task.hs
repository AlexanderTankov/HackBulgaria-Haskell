
--task 1
class Dionom d where
    dempty :: d
    dappend :: d -> d -> d
    dconcat :: [d] -> d
    dconcat xs = foldr dappend dempty xs

instance Dionom [a] where
    dempty = []
    dappend x y = x ++ y

newtype Sums a = Sums { getSum :: a }

instance Num a => Dionom (Sums a) where
    dempty = Sums 0
    dappend (Sums x) (Sums y) = Sums (x + y)

newtype Products a = Products { getProduct :: a }

instance Num a => Dionom (Products a) where
    dempty = Products 1
    dappend (Products x) (Products y) = Products (x * y)


--task 2
data List i a = Append i (List i a) (List i a) | Entry a | Nil

append :: Num i => List i a -> List i a -> List i a
append Nil l = l
append l Nil = l
append (Append x l1 l2) (Append y l3 l4) = Append (x + y) (Append x l1 l2) (Append y l3 l4)

getEntry :: Num i => i -> List i a -> Maybe Entry a
getEntry _ Nil = Nothing
getEntry 0 Entry a = Just a
getEntry i (Append x (List i1 a1) (List i2 a2))
    | i > x || i <= 0 = Nothing
    | i < i1 = getEntry i (List i1 a1)
    | otherwise = getEntry (i - i1) (List i2 a2)