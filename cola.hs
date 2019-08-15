import System.Directory

type Words = String
data Question = NoQuestion | Question Words deriving (Show, Read)
data Answer = NoAnswer | Answer Words deriving (Show, Read)
data Report = Report Question Answer deriving (Show, Read)



-- data Fan a b = ClosedFan | A a | B b | LFan a [Fan a b] | RFan b [Fan a b] deriving (Show, Read)
-- type Words = String
-- data Question = Question Words deriving (Show, Read)
-- data Answer = Answer Words deriving (Show, Read)

-- answer :: Words -> Fan Answer Question
-- answer words = LFan (Answer words) ([ClosedFan])

-- question :: Words -> Fan Answer Question
-- question words = RFan ([ClosedFan]) (Question words)

-- add :: Fan Answer Question -> Fan Answer Question -> Fan Answer Question
-- add (LFan answer questions) question@(RFan _ _) = LFan answer (question : questions)
-- add (RFan answers question) answer@(LFan _ _) = RFan (answer : answers) question

main = do
  putStrLn . show $ [ Report (Question "what") (Answer "car") ]
-- main = do
--   createFile "db.txt"
--   contents <- readFile "db.txt"
--   let remembered = recall contents
--   putStrLn (show remembered)
--   input <- getLine
--   let heard = comprehend input
--   putStrLn (show heard)
--   writeFile "db.txt" . show $ add heard remembered
--   main

-- comprehend :: Words -> Fan Answer Question
-- comprehend ('?' : words) = question words
-- comprehend words = answer words



-- recall :: String -> Fan Answer Question
-- recall contents
--   | contents == "" = ClosedFan
--   | otherwise = read contents

-- createFile :: String -> IO ()
-- createFile file = do
--   fileExist <- doesFileExist file
--   if not fileExist
--     then writeFile file ""
--     else return ()
 