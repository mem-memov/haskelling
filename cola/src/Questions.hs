module Questions (find, show) where

import MessageTypes
import qualified Question (show, hasWords)
import Prelude hiding (show)

find :: Words -> [Question] -> (Maybe Question, [Question])
find words questions = 
  foldr 
    (\question result -> 
      if Question.hasWords question words
        then (Just question, snd result) 
        else (fst result, (question : snd result))
    ) 
    (Nothing, []) 
    questions

show :: [Question] -> String
show [] = ""
show (question : otherQuestions) = "  " ++ (Question.show question) ++ "\n" ++ (show otherQuestions)
