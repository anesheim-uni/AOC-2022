-- Input: String of characters to decode
-- Output1: Index of first start-of-packet (window size 4)
-- Output2: Index of first start-of-message (window size 14)

main = do 
    let fileName = "input_files/06dec.txt"
    contents <- readFile fileName
    print $ part1 contents
    print $ part2 contents 
    
part1 :: String -> Int
part1 = generalizedSolver 4

part2 :: String -> Int
part2 = generalizedSolver 14

generalizedSolver :: Int -> String -> Int 
generalizedSolver numDistinctChars input | allUnique window = numDistinctChars
                                         | otherwise = 1 + generalizedSolver numDistinctChars (drop 1 input)
                    where window = take numDistinctChars input

allUnique :: String -> Bool 
allUnique [] = True
allUnique (c:str) | c `elem` str = False 
                  | otherwise = allUnique str