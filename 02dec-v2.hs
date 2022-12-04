import qualified Text.Read as R
-- Input: Strategy guide 
-- Output1: Expected points of applying strategy as infered from input
-- Output2: Expected points of applying strategy as intended by the elf

data Outcome = Loss | Draw | Win 
    deriving (Show, Eq)


data Symbol = Rock | Paper | Scissor
    deriving (Show, Enum, Eq)

instance Ord Symbol where 
    compare Rock Paper = LT 
    compare Rock Scissor = GT 
    compare Paper Rock = GT
    compare Paper Scissor = LT
    compare Scissor Rock = LT 
    compare Scissor Paper = GT 
    compare _ _ = EQ


main = do 
    let fileName = "input_files/02dec.txt"
    content <- readFile fileName
    let strategy = lines content
    print $ part1 $ fixture1 strategy
    print $ part2 $ fixture2 strategy

part1 :: [(Symbol, Symbol)] -> Int 
part1 = go 0
    where go acc ((opp, me):rest) | opp < me = go (acc + 6 + move) rest
                                  | opp == me = go (acc + 3 + move) rest 
                                  | otherwise = go (acc + move) rest
                                  where move = sym2int me
          go acc [] = acc

part2 :: [(Symbol, Outcome)] -> Int 
part2 = go 0
    where go acc ((opp, outcome):rest) = go (acc + move) rest
              where move | outcome == Loss = sym2int $ loseAgainst opp 
                         | outcome == Draw = 3 + sym2int opp 
                         | otherwise = 6 + sym2int (winAgainst opp)
          go acc [] = acc

fixture1 :: [String] -> [(Symbol, Symbol)]
fixture1 = map (toFixturePair . words)
      where toFixturePair :: [String] -> (Symbol, Symbol)
            toFixturePair [opp, me] = mapPair str2sym str2sym (opp, me)
            toFixturePair _ = undefined

fixture2 :: [String] -> [(Symbol, Outcome)]
fixture2 = map (toFixturePair . words)
      where toFixturePair :: [String] -> (Symbol, Outcome)
            toFixturePair [opp, me] = mapPair str2sym str2outcome (opp, me)
            toFixturePair _ = undefined
mapPair f g (a, b) = (f a, g b)

loseAgainst :: Symbol -> Symbol 
loseAgainst sym | sym == Rock = Scissor 
                | otherwise = pred sym

winAgainst :: Symbol -> Symbol 
winAgainst sym | sym == Scissor = Rock 
               | otherwise = succ sym

str2sym :: String -> Symbol 
str2sym str | str == "A" || str == "X" = Rock
            | str == "B" || str == "Y" = Paper 
            | otherwise  = Scissor

str2outcome :: String -> Outcome
str2outcome str | str == "X" = Loss 
                | str == "Y" = Draw
                | otherwise = Win


sym2int :: Symbol -> Int 
sym2int sym | sym == Rock = 1
            | sym == Paper = 2
            | sym == Scissor = 3