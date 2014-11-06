Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']


 encode :: (Eq a) => [a] -> [(Int, a)]

> data Some a = Multiple Int a | Single a deriving Show
> encodeModified :: (Eq a) => [a] -> [Some a]
> encodeModified []     = []
> encodeModified [x]    = [Single x]
> encodeModified (x:xs) = count (x:first) : encodeModified rest
>  where 
>   reps [] = ([], [])
>   reps (y:ys)
>    | (y == x) = (y:f, r)
>    | otherwise = ([], (y:ys))
>     where 
>      (f, r) = reps ys
>   (first, rest) = reps xs
>   count :: [a] -> Some a
>   count xm = Multiple (length xm) (head xm)


Problem 12

(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"

> decodeModified :: [Some a] -> [a]
> decodeModified [] = []
> decodeModified (x:xs) = helper x ++ decodeModified(xs)
>   where 
>     helper :: Some a -> [a]
>     helper (Single w) = [w]
>     helper (Multiple n w) = take n $ repeat w

Problem 13

(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

Problem 14

(*) Duplicate the elements of a list.

Example in Haskell:

dupli [1, 2, 3]
[1,1,2,2,3,3]

> dupli :: [a] -> [a]
> dupli [] = []
> dupli (x:xs) = (take 2 $ repeat x) ++ dupli(xs)

Problem 15

(**) Replicate the elements of a list a given number of times.

Example in Haskell:

repli "abc" 3
"aaabbbccc"

> repli :: [a] -> Int -> [a]
> repli [] _ = []
> repli (x:xs) i = (take i $ repeat x) ++ repli xs i

Problem 16

(**) Drop every N'th element from a list.

Example in Haskell:

*Main> dropEvery "abcdefghik" 3 # 3amice: Haskellians can't do the alphabet.
"abdeghk"

> dropEvery :: [a] -> Int -> [a]
> dropEvery x n = helper x n
>  where
>   helper [] _ = []
>   helper (_:xs) 1 = helper xs n
>   helper (x:xs) i = [x] ++ helper xs (i-1)

Problem 17

(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")

 split :: [a] -> Int -> [[a]]

> split :: [a] -> Int -> [[a]]
> split xs i = splitHelper xs [] i
>   where
>   splitHelper :: [a] -> [a] -> Int -> [[a]]
>   splitHelper (x:xs) acc 0 = acc : (x:xs) : []
>   splitHelper (x:xs) acc i = splitHelper (xs) (acc ++ [x]) (i-1)
>   splithelper _ _ _         = error "wtf are you doing mate"



Problem 18

(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example in Haskell:

*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"

> slice :: [a] -> Int -> Int -> [a]
> slice _ _ 2       = []
> slice (x:xs) 1 t  = x : slice xs 1 (t-1)
> slice (_:xs) i t      
>   | i < t     = slice xs (i-1) t
>   | otherwise = error "what's going on"

Problem 19

(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples in Haskell:

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"

Problem 20

(*) Remove the K'th element from a list.

Example in Prolog:

?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]

Example in Lisp:

* (remove-at '(a b c d) 2)
(A C D)

(Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

Example in Haskell:

*Main> removeAt 2 "abcd"
('b',"acd")
