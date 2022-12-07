import Data.Char (isDigit, isLetter, isSpace)
import Data.List (transpose, isPrefixOf, insert, nubBy, sort)
import Data.Function (on)

-- Input: Stacks and move instructions
-- Output1: String corresponding to top-of-stack items after complete instructions
-- Output2: String corresponding to top-of-stack items after complete instructions, preserving new ordering of moves.

type Crate = Char 
type Stack = (Int, [Crate])
type Index = Int
type Amount = Int 
type Instruction = (Amount, Index, Index)

main = do 
    let fileName = "input_files/05dec.txt"
    contents <- readFile fileName
    let (stacks, instructions) = parseRaw contents 
    print $ sort $ part1 $ parseRaw contents
    print $ sort $ part2 $ parseRaw contents

part1 :: ([Stack], [Instruction]) -> [Stack]
part1 ([], []) = []
part1 (stack:rest, []) = stack : part1 (rest, []) 
part1 (stacks, instruction:rest) = part1 (crates, rest)
          where (srcStack, destStack) = getRelevantStacks getStack (srcId, destId) stacks
                (amount, srcId, destId) = instruction
                (srcStack', destStack') = multiMove amount (srcStack, destStack)
                crates = resolveStacks stacks (srcStack', destStack')

part2 :: ([Stack], [Instruction]) -> [Stack]
part2 ([], []) = []
part2 (stack:rest, []) = stack : part2 (rest, []) 
part2 (stacks, instruction:rest) = part2 (crates, rest)
          where (srcStack, destStack) = getRelevantStacks getStack (srcId, destId) stacks
                (amount, srcId, destId) = instruction
                (srcStack', destStack') = multiMove2 amount (srcStack, destStack)
                crates = resolveStacks stacks (srcStack', destStack')

resolveStacks :: [Stack] -> (Stack, Stack) -> [Stack]
resolveStacks base (srcStack, destStack) = nubBy ((==) `on` fst) stacks 
            where stacks = srcStack : destStack : base

getRelevantStacks :: (Index -> [Stack] -> Stack) -> (Index, Index) -> [Stack] -> (Stack, Stack)
getRelevantStacks f (srcId, destId) stacks = (f srcId stacks, f destId stacks)

multiMove :: Index -> (Stack, Stack) -> (Stack, Stack)
multiMove n (srcStack, destStack) = (srcStack', destStack')
        where crates = take n $ snd srcStack 
              srcStack' = (fst srcStack, drop n $ snd srcStack)
              destStack' = (fst destStack, reverse crates ++ snd destStack)

multiMove2 :: Index -> (Stack, Stack) -> (Stack, Stack)
multiMove2 n (srcStack, destStack) = (srcStack', destStack')
        where crates = take n $ snd srcStack 
              srcStack' = (fst srcStack, drop n $ snd srcStack)
              destStack' = (fst destStack, crates ++ snd destStack)

getStack :: Index -> [Stack] -> Stack 
getStack index ((idx, items):stacks) | idx == index = (idx, items)
                                     | otherwise = getStack index stacks-- Mby bad way to handle not found stack - shouldnt happen tho.
getStack _ [] = undefined

topOfStack :: Stack -> Crate
topOfStack = head . snd

----------------------------- Parsing! -----------------------------
parseRaw :: String -> ([Stack], [Instruction])
parseRaw str = (stacks, instructions)
          where (rawStacks, rawInstructions) = splitStacksFromMoves str
                stacks = loadStacks rawStacks
                instructions = loadInstructions rawInstructions

loadInstructions :: [String] -> [Instruction]
loadInstructions input = map (str2instruction . removeSpaces) (concatMap lines input)

loadStacks :: [String] -> [Stack]
loadStacks input = map str2stack $ concatMap words $ clean $ transpose input

str2instruction :: String -> Instruction
str2instruction = go (0, 0, 0) 
      where go :: Instruction -> String -> Instruction
            go (0, _, _) str = let amount = readInt amountStr
                                   amountStr = takeWhile isDigit $ drop 4 str
                                   --toDrop = length $ dropWhile isLetter str
                               in go (amount, 0, 0) $ drop (4 + length amountStr) str
            go (amount, 0, _) str = let src = readInt srcStr
                                        srcStr = takeWhile isDigit $ drop 4 str
                                    in go (amount, src, 0) $ drop (4 + length srcStr) str
            go (amount, src, 0) str = let dest = readInt destStr
                                          destStr = takeWhile isDigit $ drop 2 str
                                      in (amount, src, dest)
            go ins _ = ins
                        

str2stack :: String -> Stack 
str2stack str = (index, crates)
      where index = readInt (takeWhile isDigit str)
            crates = reverse $ filter isLetter str

clean :: [String] -> [String]
clean [] = []
clean ((' ':_):next) = clean next
clean (x:xs) = x : clean xs 

removeSpaces ::  String -> String
removeSpaces = filter (not . isSpace)

readInt :: String -> Int 
readInt = read

splitStacksFromMoves:: String -> ([String], [String])
splitStacksFromMoves = go ([], []) 
          where go :: ([String], [String]) -> String -> ([String], [String])
                go (stacks, []) ('\n':'\n':rest) = (stacks, [rest]) 
                go (stacks, []) ('\n':rest) = 
                                              let line = takeWhile (/= '\n') rest 
                                                  rem  = drop (length line) rest
                                              in go (line:stacks, []) rem
                go (stacks, []) str =  let line = takeWhile (/= '\n') str 
                                           rem  = drop (length line) str
                                       in go (line:stacks, []) rem

