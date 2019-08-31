import System.Directory


data LR l r = N | L l [LR l r] | R r [LR l r] deriving (Show, Read)
type Words = String
data Question = Question Words deriving (Show, Read)
data Answer = Answer Words deriving (Show, Read)

answer :: Words -> LR Answer Question
answer words = L (Answer words) [N]

question :: Words -> LR Answer Question
question words = R (Question words) [N]

add :: LR Answer Question -> LR Answer Question -> LR Answer Question
add (L answer questions) question@(R _ _) = L answer (question : questions)
add (R question answers) answer@(L _ _) = R question (answer : answers) 
add N question@(R _ _) = question
add N answer@(L _ _) = answer
add _ _ = N
-- add answer@(L _ _) (L answer questions) = 

main = do
  createFile "db.txt"
  contents <- readFile "db.txt"
  let remembered = recall contents
  putStrLn (show remembered)
  input <- getLine
  let heard = comprehend input
  putStrLn (show heard)
  writeFile "db.txt" . show $ add remembered heard
  main

comprehend :: Words -> LR Answer Question
comprehend ('?' : words) = question words
comprehend words = answer words


recall :: String -> LR Answer Question
recall contents
  | contents == "" = N
  | otherwise = read contents

createFile :: String -> IO ()
createFile file = do
  fileExist <- doesFileExist file
  if not fileExist
    then writeFile file ""
    else return ()
 