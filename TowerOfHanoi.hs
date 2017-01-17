type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)] --base case for moving a disc
--recursively move n-1 discs from a to c with b as temp storage, then move top disc from a to b, then move n-1 discs from c to b using a as temporary storage
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a