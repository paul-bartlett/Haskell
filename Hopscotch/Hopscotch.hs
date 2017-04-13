-- Takes a list and returns a list of lists with the nth element only containing every nth character
skips :: [a] -> [[a]]
skips str = map (every str) [1..length str]

-- Drops n-1 elements after the first recursively
every :: [a] -> Int -> [a]
every xs n = case drop (n-1) xs of
    (y:ys) -> y : every ys n
    [] -> []