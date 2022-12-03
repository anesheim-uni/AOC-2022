import qualified Text.Read as R
import Text.Parsec (parse)
-- Input: Strategy guide 
-- Output1: Expected points of applied strategy 
-- Output2: 

main = do 
    let fileName = "input_files/02dec.txt"
    content <- readFile fileName
    let strategy = lines content
    let pointDict = [("A", 1), ("B", 2), ("C", 3), ("X", 1), ("Y", 2), ("Z", 3)]
    print $ part1 $ parseFixture (fixture strategy) pointDict
    print $ part2 $ parseFixture (fixture strategy) pointDict

part1 :: [(Int, Int)] -> Int 
part1 = go 0
    where go acc ((opp, me):rest) | won = go (acc + 6 + me) rest
                                  | opp == me = go (acc + 3 + me) rest 
                                  | otherwise = go (acc + me) rest
                                  where won = opp - me == 2 || opp - me == -1
          go acc [] = acc

part2 :: [(Int, Int)] -> Int 
part2 = go 0
    where go acc ((opp, cond):rest) 
              | cond == 1 && opp /= 1 = go (acc + opp - 1) rest
              | cond == 1 = go (acc + 3) rest
              | cond == 2 = go (acc + 3 + opp) rest 
              | cond == 3 && opp /= 3 = go (acc + 6 + opp + 1) rest
              | otherwise = go (acc + 6 + 1) rest
          go acc [] = acc

--fixture :: [String] -> [(Int, Int)]
fixture = map (toFixturePair . words)
      where --toFixturePair :: [String] -> (Int, Int)
            toFixturePair [opp, me] = (opp, me)
            toFixturePair _ = undefined

parseFixture :: [(String, String)] -> [(String, Int)] -> [(Int, Int)]
parseFixture _ [] = []
parseFixture [] _ = []
parseFixture ((opp, me):rest) points = (sign2int opp points, sign2int me points) :  parseFixture rest points

sign2int :: String -> [(String, Int)] -> Int 
sign2int s dict = case lookup s dict of
        Nothing -> 0
        Just v -> v
