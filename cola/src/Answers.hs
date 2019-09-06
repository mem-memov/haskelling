module Answers (find, show) where

import MessageTypes
import qualified Answer (show, hasWords)
import qualified Pick (make, hold, append)
import Prelude hiding (show)

find :: Words -> [Answer] -> Pick Answer
find words answers = 
  foldr 
    (\answer pick -> 
      if Answer.hasWords answer words
        then Pick.hold pick answer
        else Pick.append pick answer
    ) 
    Pick.make
    answers

show :: [Answer] -> String
show [] = ""
show (answer : otherAnswers) = "  " ++ (Answer.show answer) ++ "\n" ++ (show otherAnswers)
