module Answers (find, show) where

import MessageTypes
import qualified Answer (show, hasWords)
import Prelude hiding (show)

find :: Words -> [Answer] -> (Maybe Answer, [Answer])
find words answers = 
  foldr 
    (\answer result -> 
      if Answer.hasWords answer words
        then (Just answer, snd result) 
        else (fst result, (answer : snd result))
    ) 
    (Nothing, []) 
    answers

show :: [Answer] -> String
show [] = ""
show (answer : otherAnswers) = "  " ++ (Answer.show answer) ++ "\n" ++ (show otherAnswers)
