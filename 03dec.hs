import Data.Char (ord, isLower)
import Data.List (intersect)

-- Input: List of strings
-- Output1: Sum of priorities of type of each rucksack
-- Output2: Sum of priorities of shared type between groups of 3 elves

main = do 
    let fileName = "input_files/03dec.txt"
    content <- readFile fileName
    let rucksacks = lines content 
    print $ part1 rucksacks 
    print $ part2 $ group3 rucksacks 

part1 :: [String] -> Int
part1 loadedSacks = sum $ map (priority . inBoth) compartments
        where compartments = split loadedSacks

part2 :: [(String, String, String)] -> Int 
part2 elfGroups = sum $ map (priority . inThree) elfGroups

split :: [String] -> [(String, String)]
split [] = []
split (r:rest) = x : split rest 
                      where mid = length r `div` 2
                            x =  splitAt mid r

inBoth :: (String, String) -> Char 
inBoth (cmp1, cmp2) = head $ cmp1 `intersect` cmp2

inThree :: (String, String, String) -> Char 
inThree (r1, r2, r3) = head $ r1 `intersect` (r2 `intersect` r3)

group3 :: [String] -> [(String, String, String)]
group3 = group 3 
    
group :: Int -> [String] -> [(String, String, String)] 
group _ [] = []
group n ls | n > 0 = (a,b,c): group n (drop n ls)
           | otherwise = group 3 [] 
           where [a,b,c] = take 3 ls
                 [] = []

priority :: Char -> Int
priority c | isLower c = ord c - ord 'a' + 1
           | otherwise = ord c - ord 'A' + 27
