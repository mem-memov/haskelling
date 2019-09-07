module AnswerFocus(show) where

import MessageTypes
import qualified Answer (show)
import qualified Question (show)
import qualified Questions (show)
import Prelude hiding (show)

show :: Answer -> String
show answer@(Answer Nothing _ answerQuestions) = "-\n -> " ++ (Answer.show answer) ++ "\n" ++ (Questions.show answerQuestions)
show answer@(Answer (Just answerQuestion) _ answerQuestions) = (Question.show answerQuestion) ++ "\n -> " ++ (Answer.show answer) ++ "\n" ++ (Questions.show answerQuestions)
