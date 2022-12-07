-- Input: String of characters to decode
-- Output1: Index of first start-of-packet
-- Output2: Index of first start-of-message



main = do 
    let fileName = "input_files/06dec.txt"
    contents <- readFile fileName
    print $ part1 4 contents
    print $ part2 14 contents



part1 :: Int -> String -> Int
part1 index input | allUnique last4 = index
                  | otherwise = part1 (index+1) $ drop 1 input 
    where last4 = take 4 input 

part2 :: Int -> String -> Int
part2 index input | allUnique last14 = index
                  | otherwise = part2 (index+1) $ drop 1 input 
    where last14 = take 14 input 

allUnique :: String -> Bool 
allUnique [] = True
allUnique (c:str) | c `elem` str = False 
                  | otherwise = allUnique str