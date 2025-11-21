-- pex6.hs 
-- unKnot Haskell

-- name: Skyler Wuest

{- DOCUMENTATION:
Will Kozma told me that in order to do the typeIIknot I needed to add a listLength function (which I made it hw6)
as well as a findPair, and then a remove2 function. In the findPair function it wasnt recursive at first but worked 
so he suggested that I put (start:(typeIknot (next:end))) to make it recursive and to actually make it go through all 
the typeIknots instead of just removing one. 

He helped my with the findPair function, telling me what types to pass in and return, walking me through
the code. He also explained to me how the indexing worked for the findPair function and he gave me the 
findPair (_:[]) _ _ = -1 base case.

For the remove2AtIndex function that he helped me with, he drew the whole thing out on the board,
explaining conceptually how it takes the two out of the list and then concatenates them together.

my typeIIknot code was only removing one pair of the typeIIknot because my otherwise was wrong and he
said to add all the logic I had before into the remove2AtIndex function at the end of the typeIIknot otherwise case

I got my test cases from the teams page.
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null (autoRunUntangle(tripCode)) = "unknot" 
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

letter :: (Char, Char) -> Char
letter = fst

listLength :: [a] -> Int
listLength []= 0
listLength (strt:end) = 1 + listLength(end)

-- Tells whether the string is going under or over (up/down uD)
uD :: (Char, Char) -> Char
uD = snd

-- Looks for typeI knot 
typeIknot :: [(Char, Char)] -> [(Char, Char)] -- returns edited list 
typeIknot[] = []
typeIknot[x] = [x]
typeIknot (start:next:end) 
    | letter start == letter next && uD start /= uD next = typeIknot end -- return list but without the two that was just checked
    | otherwise = (start:(typeIknot (next:end))) -- if a typeIknot is not found return the same list

-- returns the index at which the pair is located at
findPair :: [(Char, Char)] -> [Char] -> Int -> Int
findPair [] _ _ = -1
findPair (_:[]) _ _ = -1
findPair (start:next:end) [first, second] index
   | (letter start == second && letter next == first) || (letter start == first && letter next == second) = index + 1
   | otherwise = findPair(next:end) [first, second] (index + 1)
findPair (start:end) [first, second] index = -1

-- removes the two that have a corresponding two 
remove2AtIndex :: Int -> [a] -> [a]
remove2AtIndex index list =
    (take (index-1) list) ++ (drop(index + 1) list) -- concatenates the list once done

-- checks for typeIIknot 
typeIIknot :: [(Char, Char)] -> [(Char, Char)]
typeIIknot [] = []
typeIIknot [x] = [x]
typeIIknot (start:next:end) 
    | findPair end ((letter start) : [letter next]) 0 == -1 = start:(typeIIknot (next:end)) -- if no pair is found return the list as is for later comparison in unKnot, this is a recursive call that goes through the entire tripcode
    | otherwise = remove2AtIndex (findPair end ((letter start) : [letter next]) 0) end -- removes the two pairs wherever they are in the list 

autoRunUntangle :: [(Char, Char)] -> [(Char, Char)]
autoRunUntangle [] = []
autoRunUntangle [x] = [x]
autoRunUntangle list = (typeIknot(typeIIknot(typeIknot(typeIIknot(list))))) -- didnt have enough time to figure out how to do this another way

main :: IO ()
main = do
   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)
   
   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " ) -- "Unkot"
   print(t02)
   print("   result:" ++ unKnot t02) -- "Unkot"
 
   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03) -- "Unkot"
 
   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04) -- "Unkot"
