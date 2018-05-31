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
foldTree = foldr insertTree Leaf

-- Inserts a new LogMessage into an existing sorted MessageTree
insertTree :: a -> Tree a -> Tree a
insertTree val Leaf = Node 0 (Leaf) val (Leaf)
insertTree val (Node n l x r)
    | heightL > heightR = Node n     l val insertR
    | heightL < heightR = Node n     insertL val r
    | otherwise         = Node (h+1) insertL val r
    where heightL = heightTree l 
          heightR = heightTree r
          insertL = insertTree x l
          insertR = insertTree x r
          h       = heightTree insertR 

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n l val r) = n