module Questions (
  findQuestion,
  showQuestions
) where

import MessageTypes
import Question

findQuestion :: Words -> [Question] -> (Maybe Question, [Question])
findQuestion words questions = 
  foldl 
    (\result question -> 
      if questionHasWords question words
        then (Just question, snd result) 
        else (fst result, (question : snd result))
    ) 
    (Nothing, []) 
    questions

showQuestions :: [Question] -> String
showQuestions [] = ""
showQuestions (question : otherQuestions) = "  " ++ (showQuestion question) ++ "\n" ++ (showQuestions otherQuestions)
