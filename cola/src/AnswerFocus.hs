module AnswerFocus(
  showAnswerFocus
) where

import MessageTypes
import Answer
import Answers
import Question
import Questions

showAnswerFocus :: Answer -> String
showAnswerFocus answer@(Answer Nothing _ answerQuestions) = " -> " ++ (showAnswer answer) ++ "\n" ++ (showQuestions answerQuestions)
showAnswerFocus answer@(Answer (Just answerQuestion) _ answerQuestions) = (showQuestion answerQuestion) ++ "\n -> " ++ (showAnswer answer) ++ "\n" ++ (showQuestions answerQuestions)
