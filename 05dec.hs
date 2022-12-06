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
    let (_, rawInstructions) = splitStacksFromMoves contents
    print $ map (str2instruction . removeSpaces) (concatMap lines rawInstructions) 




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
            crates = filter isLetter str

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

