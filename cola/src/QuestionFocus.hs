module QuestionFocus(
  showQuestionFocus
) where

import MessageTypes
import Answer
import Answers
import Question
import Questions

showQuestionFocus :: Question -> String
showQuestionFocus question@(Question Nothing _ questionAnswers) = " -> " ++ (showQuestion question) ++ "\n" ++ (showAnswers questionAnswers)
showQuestionFocus question@(Question (Just questionAnswer) _ questionAnswers) = (showAnswer questionAnswer) ++ "\n -> " ++ (showQuestion question) ++ "\n" ++ (showAnswers questionAnswers)
