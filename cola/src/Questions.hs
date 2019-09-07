module Questions (find, show) where

import MessageTypes
import qualified Question (show, hasWords)
import qualified Pick (make, collect)
import Prelude hiding (show)

find :: Words -> [Question] -> Pick Question
find words questions = Pick.collect Pick.make questions (Question.hasWords words)

show :: [Question] -> String
show [] = ""
show (question : otherQuestions) = " " ++ (Question.show question) ++ "\n" ++ (show otherQuestions)
