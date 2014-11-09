> main = putStrLn "hello!"

Problem 1

(*) Find the last element of a list.

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'

Solution:

> myLast :: [a] -> a
> myLast [x] = x
> myLast (_:xs) = myLast xs
> myLast [] = error "Empty list? xD"

Problem 2

(*) Find the last but one element of a list.

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

> myButLast :: [a] -> a
> myButLast [x, _] = x
> myButLast (_:xs) = myButLast xs
> myButLast _      = error "1 or 0 elements in list? xD"


Problem 3

(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c

Example in Haskell:

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'

> elementAt :: [a] -> Int -> a
> elementAt (x:xs) 1  = x
> elementAt (_:xs) i  = elementAt xs (i-1)
> elementAt _ _       = error "hmm... couldn't match...? xD"

Problem 4

(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13

> myLength :: [a] -> Int
> myLength []     = 0
> myLength (_:xs) = 1 + myLength xs 

myLength _      = error "Um if you find this case mail me at github (3amice) because I am interested xD"

Problem 5

(*) Reverse a list.

Example in Haskell:

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]

> myReverse :: [a] -> [a]
> myReverse [x]    = [x]
> myReverse (x:xs) = myReverse(xs) ++ [x]

Problem 6

(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True

> isPalindrome :: (Eq a) => [a] -> Bool
> isPalindrome xs = myReverse xs == xs

Problem 7

(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.


*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]

> data NestedList a = Elem a | List [NestedList a]
> flatten :: NestedList a -> [a]
> flatten nl = flattenHelper nl []
>  where 
>   flattenHelper :: NestedList a -> [a] -> [a]
>   flattenHelper (List []) acc = acc
>   flattenHelper (Elem x) acc = acc ++ [x]
>   flattenHelper (List (x:xs)) acc = flattenHelper (List xs) (flattenHelper x acc) 

Problem 8

(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example in Haskell:

compress "aaaabccaadeeee"
"abcade"

> compress :: (Eq a) => [a] -> [a]
> compress [] = []
> compress [x] = [x]
> compress (x1:x2:xs) 
>        | x1 == x2   = compress $ [x1] ++ xs
>        | otherwise  = [x1] ++ compress([x2] ++ xs)

Problem 9

(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]

> pack :: Eq a => [a] -> [[a]]
> pack [] = []
> pack [xs] = [[xs]]
> pack (x:xs) = (x:first) : pack rest
>   where 
>         reps [] = ([], [])
>         reps (y:ys)
>           | (y == x) = (y:f, r)
>           | otherwise = ([], (y:ys))
>            where 
>             (f, r) = reps ys
>         (first, rest) = reps xs

Problem 10

(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

> encode :: (Eq a) => [a] -> [(Int, a)]
> encode = (map (\i -> (length i, head i))) . pack




