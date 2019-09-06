module Answers (
  find,
  showAnswers
) where

import MessageTypes
import Answer

find :: Words -> [Answer] -> (Maybe Answer, [Answer])
find words answers = 
  foldr 
    (\answer result -> 
      if answerHasWords answer words
        then (Just answer, snd result) 
        else (fst result, (answer : snd result))
    ) 
    (Nothing, []) 
    answers

showAnswers :: [Answer] -> String
showAnswers [] = ""
showAnswers (answer : otherAnswers) = "  " ++ (showAnswer answer) ++ "\n" ++ (showAnswers otherAnswers)
