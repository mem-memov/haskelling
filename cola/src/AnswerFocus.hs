module AnswerFocus(
  showAnswerFocus
) where

import MessageTypes
import qualified Answer (show)
import qualified Question (show)
import qualified Questions (show)

showAnswerFocus :: Answer -> String
showAnswerFocus answer@(Answer Nothing _ answerQuestions) = " -> " ++ (Answer.show answer) ++ "\n" ++ (Questions.show answerQuestions)
showAnswerFocus answer@(Answer (Just answerQuestion) _ answerQuestions) = (Question.show answerQuestion) ++ "\n -> " ++ (Answer.show answer) ++ "\n" ++ (Questions.show answerQuestions)
