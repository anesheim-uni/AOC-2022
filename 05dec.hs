import Data.Char (isDigit, isLetter, isSpace)
import Data.List (transpose, isPrefixOf)
-- Input: Stacks and move instructions
-- Output1: String corresponding to top-of-stack items after complete instructions
-- Output2: 

type Stack = (Int, [Char])
type Index = Int
type Amount = Int 
type Instruction = (Amount, Index, Index)

main = do 
    let fileName = "input_files/05dec.txt"
    contents <- readFile fileName
    let (stacks, instructions) = parseRaw contents 
    --print $ take 5 $ lines contents 
    print instructions 

parseRaw :: String -> ([Stack], [Instruction])
parseRaw str = (stacks, instructions)
          where (rawStacks, rawInstructions) = splitStacksFromMoves $ lines str
                stacks = loadStacks rawStacks
                instructions = loadInstructions rawInstructions

loadInstructions :: [String] -> [Instruction]
loadInstructions = map str2instruction 

loadStacks :: [String] -> [Stack]
loadStacks input = map str2stack $ transpose input

str2instruction :: String -> Instruction
str2instruction = go (0, 0, 0) 
      where go :: Instruction -> String -> Instruction
            go (0, _, _) str = let amount = readInt amountStr
                                   amountStr = takeWhile isDigit str
                               in go (amount, 0, 0) $ drop (length amountStr + 1) str
            go (amount, 0, _) str = let src = readInt srcStr
                                        srcStr = takeWhile isDigit str
                                    in go (amount, 0, 0) $ drop (length srcStr + 1) str
            go (amount, src, 0) str = let dest = readInt destStr
                                          destStr = takeWhile isDigit str
                                      in go (amount, 0, 0) $ drop (length destStr + 1) str
            go (amount, src, dest) _ = (amount, src, dest)

str2stack :: String -> Stack 
str2stack str = (index, crates)
      where index = readInt (takeWhile isDigit str)
            crates = filter isLetter str

readInt :: String -> Int 
readInt = read

splitStacksFromMoves:: [String] -> ([String], [String])
splitStacksFromMoves = go ([], [])
          where --go :: ([String], [String]) -> [String] -> ([String], [String])
                go (stacks, []) ((x:xs):rest) | x == '[' || x == ' '  = go ((x:xs) : stacks, []) rest
                                              | x == '\n' = go (stacks, rest) rest 
                go (stacks, rest) _ = (stacks, rest)


