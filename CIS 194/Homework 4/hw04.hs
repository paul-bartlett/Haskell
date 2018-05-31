-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x    = (x - 2) * fun1 xs
   | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Exercise 2
data Tree a = Leaf
			| Node Integer (Tree a) a (Tree a)
	deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr1 (\x -> insert x)

-- Inserts a new LogMessage into an existing sorted MessageTree
insert :: Integer -> a -> Tree a
insert height val =
insert lg@(LogMessage _ _ _) Leaf = Node Leaf lg Leaf
insert log1@(LogMessage _ time1 _) (Node l log2@(LogMessage _ time2 _) r)
    | time1 > time2 = Node l log2 (insert log1 r)
    | otherwise     = Node (insert log1 l) log2 r
insert _ tree = tree