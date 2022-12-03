import qualified Text.Read as R
import qualified Data.List as L
-- Input: String of numbers, newlines and blank lines 
-- Output, part 1: Highest calories of elf 
-- Output, part 2: Sum of the 3 highest calories

main = do 
    let fileName = "input_files/01dec-1.txt"
    content <- readFile fileName
    let calories = map readInt $ lines content
    print . part1 $ caloriesByElf calories
    print . part2 $ caloriesByElf calories 

part1 :: [Int] -> Int 
part1 = maximum 

caloriesByElf :: [Int] -> [Int]
caloriesByElf = go 0 
              where go acc (curr:rest) | curr /= 0 = go (acc + curr) rest 
                                       | otherwise = acc : caloriesByElf rest
                    go acc [] = [acc]


part2 :: [Int] -> Int
part2 = sum . take 3 . L.sortBy (flip compare) 

readInt :: String -> Int
readInt s = case R.readMaybe s of 
          Nothing -> 0 
          Just v -> v


