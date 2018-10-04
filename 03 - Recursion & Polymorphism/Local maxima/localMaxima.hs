-- Finds all local maxima in a list of integers
localMaxima :: [Integer] -> [Integer]
localMaxima []       = []
localMaxima (x:[])   = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:zs)
    | y > x && y > z = y : localMaxima (y:z:zs)
    | otherwise      = localMaxima (y:z:zs)