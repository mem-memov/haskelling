module Questions (find, show) where

import MessageTypes
import qualified Question (show, hasWords)
import qualified Pick (make, hold, append)
import Prelude hiding (show)

find :: Words -> [Question] -> Pick Question
find words questions = 
  foldr 
    (\question pick -> 
      if Question.hasWords question words
        then Pick.hold pick question
        else Pick.append pick question
    ) 
    Pick.make
    questions

show :: [Question] -> String
show [] = ""
show (question : otherQuestions) = "  " ++ (Question.show question) ++ "\n" ++ (show otherQuestions)
