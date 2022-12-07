-- Input: String of commands and outputs of said commands 
-- Output1: Total size of directories whose size is at most 100.000 
-- Output2: 



main = do 
    let fileName = "input_files/07dec.txt"
    contents <- readFile fileName
    print $ take 5 $ lines contents