type Words = String
data Question = Question Words [Answer] deriving Show
data Answer = Answer Words [Question] deriving Show

answer :: Words -> Answer
answer words = Answer words []

question :: Words -> Question
question words = Question words []

ask :: Answer -> Question -> Answer
ask (Answer words questions) question = Answer words (question : questions)

reply :: Question -> Answer -> Question
reply (Question words answers) answer = Question words (answer : answers)

main = interact play

play :: String -> String
play = unlines . map (\xs -> "------------") . lines   