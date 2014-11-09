> import System.Random 
> import Control.Monad (replicateM)
> import Data.List


Problem 21

Insert an element at a given position into a list.

Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"

> insertAt :: a -> [a] -> Int -> [a]
> insertAt x xs i = take (i-1) xs ++ [x] ++ (reverse $ take (length xs - i + 1) $ reverse xs)

> insertAt' :: a -> [a] -> Int -> [a]
> insertAt' x xs i = intro ++ [x] ++ rest
>   where
>     intro = take (i-1) xs
>     rest  = reverse $ take (length xs - i + 1) $ reverse xs

> insertAt'' :: a -> [a] -> Int -> [a]
> insertAt'' e xs 1     = e : xs
> insertAt'' e (x:xs) n = x : (insertAt'' e xs (n-1))
> insertAt'' _ _ _      = error "sorry."

Problem 22

Create a list containing all integers within a given range.

Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]

> range :: Int -> Int -> [Int]
> range x y 
>  | x == y = [x]
>  | x < y = x : (range (x+1) y)
>  | otherwise = error "lol... don't try it xD"



Problem 23

Extract a given number of randomly selected elements from a list.


Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda

> rnd_select xs n 
>    | n < 0     = error "N must be greater than zero."
>    | otherwise = replicateM n rand
>        where rand = do r <- randomRIO (0, (length xs) - 1)
>                        return (xs !! r)

Problem 24

Lotto: Draw N different random numbers from the set 1..M.

Example in Haskell:

Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]

> diff_select ::  Int -> Int -> IO [Int]
> diff_select n m = rnd_select [1..m] n 

Problem 25

Generate a random permutation of the elements of a list.

Example in Haskell:

Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"

> rndElem :: [a] -> IO a
> rndElem xs = do
>   index <- randomRIO (0, length xs - 1)
>   return $ xs !! index
 
> rndPermutation :: [a] -> IO [a]
> rndPermutation xs = rndElem . permutations $ xs

Solutions


Problem 26

(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example in Haskell:

combinations 3 "abcdef"
["abc","abd","abe",...]

> combinations :: Int -> [a] -> [[a]]
> combinations k = filter pred . subsequences 
>  where
>   pred :: [a] -> Bool 
>   pred = (k==).length

Problem 27

Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial Problem in a good book on discrete mathematics under the term "multinomial coefficients".

Example in Haskell:

P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)
 
27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)

Problem 28

Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Example:

* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

Example in Haskell:

Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude>["o","de","de","mn","abc","fgh","ijkl"]

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Example in Haskell:

lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]
