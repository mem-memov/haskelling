module Answers (find, show) where

import MessageTypes
import qualified Answer (show, hasWords)
import qualified Pick (make, collect)
import Prelude hiding (show)

find :: Words -> [Answer] -> Pick Answer
find words answers = Pick.collect Pick.make answers (Answer.hasWords words)

show :: [Answer] -> String
show [] = ""
show (answer : otherAnswers) = " " ++ (Answer.show answer) ++ "\n" ++ (show otherAnswers)
