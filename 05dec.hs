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
    print $ stacks 

parseRaw :: String -> ([Stack], [Intruction])
parseRaw str = (stacks, instructions)
          where (stacks, rawInstructions) = loadStacks $ lines str
                instructions = loadInstructions rawInstructions

loadStacks :: [String] -> [Stack]
loadStacks (x:xs) = 
          where stacks = toStacks x 


