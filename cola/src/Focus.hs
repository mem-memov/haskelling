module Focus(show) where

import MessageTypes
import qualified AnswerFocus (show)
import qualified QuestionFocus (show)
import Prelude hiding (show)

show :: Message -> String
show Silence = "\n-"
show (Reference question) = "\n" ++ QuestionFocus.show question
show (Inquiry answer) = "\n" ++ AnswerFocus.show answer
