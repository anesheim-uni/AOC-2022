import Data.List (intersect)
-- Input: List of strings
-- Output1: Amount of overlapping pairs
-- Output2: 

type Pair = (Range, Range)

type Range = (Int, Int)

main = do 
    let fileName = "input_files/04dec.txt"
    contents <- readFile fileName
    let input = lines contents 
    print $ part1 input 
    print $ part2 input

part1 :: [String] -> Int 
part1 input = sum $ map overlap pairs 
    where pairs = map toPair input

part2 :: [String] -> Int 
part2 input = sum $ map overlap2 pairs 
    where pairs = map toPair input

toPair :: String -> Pair 
toPair str = (range1, range2)
    where range1 = toRange a 
          range2 = toRange b 
          [a, b] = splitOn ',' str 

toRange :: String -> Range 
toRange str = (start, end) 
    where start = str2int a
          end = str2int b 
          [a, b] = splitOn '-' str 

str2int :: String -> Int 
str2int = read 

overlap :: Pair -> Int 
overlap (r1, r2) | shared == [s1..e1] || shared == [s2..e2] = 1 
                 | otherwise = 0
                 where shared = [s1..e1] `intersect` [s2..e2]
                       (s1, e1) = r1 
                       (s2, e2) = r2 

overlap2 :: Pair -> Int 
overlap2 (r1, r2) | shared /= [] = 1 
                  | otherwise = 0
                  where shared = [s1..e1] `intersect` [s2..e2]
                        (s1, e1) = r1 
                        (s2, e2) = r2 
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn sep str = 
        let (left, right) = break (== sep) str
        in left : splitOn sep (drop 1 right)



splitOn' :: Char -> String -> (String, String)
splitOn' sep str = 
        let (left, right) = break (== sep) str
        in (left, drop 1 right)


