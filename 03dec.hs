-- Input: List of strings
-- Output1: Sum of priorities of each rucksack
-- Output2: 

input = "vJrwpWtwJgWrhcsFMMfFFhFp" --""\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nCCQTzRLzvQVVfRzJfMPsnBlglgPmBgPmvSrl\nRMfvbbszHTsssFPzDQPggpQJPQ"
main = do 
    let fileName = "input_files/03dec.txt"
    content <- readFile fileName
    let rucksacks = lines content 
    let priorities = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]
    let test = lines input 
    --print $ words input
    --print $ compartments $ lines input
    print $ duplicates $ compartments test

duplicates :: [(String, String)] -> [String] 
duplicates [] = []
duplicates [(_, [])] = []
duplicates [([], _)] = []
duplicates ((r:comp1, comp2):rest) = if r `elem` comp2 then [r] : duplicates ((comp1, comp2):rest) else duplicates $ (comp1, comp2):rest


uniques :: String -> String -> String 
uniques [] _ = []
uniques (c:rest) str = if c `elem` str then [c] else uniques rest str

compartments :: [String] -> [(String, String)]
compartments [] = []
compartments (r:rest) = splitAt mid r : compartments rest 
                      where mid = length r `div` 2
