module Answers (
  findAnswer,
  showAnswers
) where

import MessageTypes
import Answer

findAnswer :: Words -> [Answer] -> (Maybe Answer, [Answer])
findAnswer words answers = 
  foldl 
    (\result answer -> 
      if answerHasWords answer words
        then (Just answer, snd result) 
        else (fst result, (answer : snd result))
    ) 
    (Nothing, []) 
    answers

showAnswers :: [Answer] -> String
showAnswers [] = ""
showAnswers (answer : otherAnswers) = "  " ++ (showAnswer answer) ++ "\n" ++ (showAnswers otherAnswers)
