module Focus(
  showFocus
) where

import MessageTypes
import AnswerFocus
import QuestionFocus

showFocus :: Message -> String
showFocus Silence = "\n-"
showFocus (Reference question) = "\n" ++ showQuestionFocus question
showFocus (Inquiry answer) = "\n" ++ showAnswerFocus answer
