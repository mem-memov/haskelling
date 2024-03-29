module QuestionFocus(show) where

import AnswerQuestionType
import qualified Answer (show)
import qualified Answers (show)
import qualified Question (show)
import Prelude hiding (show)

show :: Question -> String
show question@(Question Nothing _ questionAnswers) = "-\n -> " ++ (Question.show question) ++ "\n" ++ (Answers.show questionAnswers)
show question@(Question (Just questionAnswer) _ questionAnswers) = (Answer.show questionAnswer) ++ "\n -> " ++ (Question.show question) ++ "\n" ++ (Answers.show questionAnswers)
