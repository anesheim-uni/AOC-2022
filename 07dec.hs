import Data.Char (isDigit, isLetter)
-- Input: String of commands and outputs of said commands 
-- Output1: Total size of directories whose size is at most 100.000 
-- Output2:

-- TYPES
type Size = Int
type Name = String
data File = File Size Name
data Dir = Dir Size Name [File]



main = do 
    let fileName = "input_files/07dec.txt"
    contents <- readFile fileName
    let data_ = parseRaw $ lines contents
    print $ take 5 data_


parseRaw :: [String] -> Dir 
parseRaw (line:rest) = case line of 
          '$':_:command -> parseCommand command
          output -> case parseOutput output of 
                  Right dir -> dir 
                  Left (File size name) -> Dir 0 "uhm" [File size name]

parseCommand :: String -> Dir
parseCommand ('c':'d':_:dirName) = Dir 0 dirName []

parseOutput :: String -> Either File Dir 
parseOutput ('d':'i':'r':dirName) = Right $ Dir 0 dirName []
parseOutput str = Left $ File (read fileSize) fileName
          where fileSize = takeWhile isDigit str
                fileName = takeWhile isLetter $ drop (length fileSize + 1) str



