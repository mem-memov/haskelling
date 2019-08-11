import System.Directory

data Fan a b = ClosedFan | A a | B b | Fan [Fan a b] [Fan a b] deriving (Show, Read)
type Words = String
data Question = Question Words deriving (Show, Read)
data Answer = Answer Words deriving (Show, Read)

answer :: Words -> Fan Answer Question
answer words = A (Answer words)

question :: Words -> Fan Answer Question
question words = B (Question words)

add :: Fan Answer Question -> Fan Answer Question -> Fan Answer Question
add answer@(A _) question@(B _) = Fan [answer] [question]
add question@(B _) answer@(A _) = Fan [answer] [question]
add (Fan [answer] questions) question@(Fan _ [one]) = Fan [answer] (question : questions)
add (Fan answers [question]) answer@(Fan [one] _) = Fan (answer : answers) [question]
add a q = Fan [a] [q]


main = do
  createFile "db.txt"
  contents <- readFile "db.txt"
  let remembered = recall contents
  putStrLn (show remembered)
  input <- getLine
  let heard = comprehend input
  putStrLn (show heard)
  writeFile "db.txt" . show $ add heard remembered
  main

comprehend :: Words -> Fan Answer Question
comprehend ('?' : words) = question words
comprehend words = answer words



recall :: String -> Fan Answer Question
recall contents
  | contents == "" = ClosedFan
  | otherwise = read contents

createFile :: String -> IO ()
createFile file = do
  fileExist <- doesFileExist file
  if not fileExist
    then writeFile file ""
    else return ()
 