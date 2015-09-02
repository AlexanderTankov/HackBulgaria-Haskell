map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' func (x:xs) = func x : map' func xs

filter' :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter' func (x:xs)
    | func x == True = x : filter' func xs
    | otherwise = filter' func xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' func acc (x:xs) = foldl' func (func acc x) xs

--task 1
-- foldl ([b] -> a -> [b]) -> [b] -> [a] -> [b]
map'' :: (a -> b) -> [a] -> [b]
map'' func l = reverse $ foldl (\acc x -> func x : acc) [] l

--map (+1) [1, 2, 3] = reverse $ foldl () [] [1, 2, 3]
--
--foldl (\acc x -> func x : acc) [] l = foldl func [2] [2, 3]
--foldl func [2] [2, 3] = foldl func (func [2] 2) [3]
--
--(func [2] 2) = (\[2] 2 -> (+1) 2 : [2]) = [3, 2]
--
--foldl func [2] [2, 3] = foldl func [3, 2] [3]
--foldl func [3, 2] [3] = foldl func [4, 3, 2] []
--foldl func [4, 3, 2] [] = [4, 3, 2]

--task 2
--foldl' :: ([a] -> a -> [a]) -> [a] -> [a] -> [a]
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' func l = foldl (\acc x -> if func x then x : acc else acc) [] l

--task 3

--task 4
repeat' :: Int -> [Int]
repeat' x = [x] ++ repeat' x
