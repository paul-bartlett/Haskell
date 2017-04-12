skips :: [a] -> [[a]]
skips str =

every n xs = case drop (n-1) xs of
    (y:ys) -> y : every n ys
    [] -> []