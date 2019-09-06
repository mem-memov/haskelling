module QuestionFocus(
  showQuestionFocus
) where

import MessageTypes
import qualified Answer (show)
import qualified Answers (show)
import qualified Question (show)

showQuestionFocus :: Question -> String
showQuestionFocus question@(Question Nothing _ questionAnswers) = " -> " ++ (Question.show question) ++ "\n" ++ (Answers.show questionAnswers)
showQuestionFocus question@(Question (Just questionAnswer) _ questionAnswers) = (Answer.show questionAnswer) ++ "\n -> " ++ (Question.show question) ++ "\n" ++ (Answers.show questionAnswers)
