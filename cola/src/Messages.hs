module Messages (add) where

import MessageTypes
import qualified Message (makeInquiry, makeReference)
import qualified Answers (find)
import qualified Questions (find)

add :: Message -> Message -> Message

add (Reference question@(Question questionAnswer questionWords questionAnswers)) (Inquiry answer@(Answer Nothing answerWords [])) = 
  case (Answers.find answerWords questionAnswers) of
    (Just (Answer foundAnswerQuestion foundAnswerWords foundAnswerQuestions), otherAnswers) -> 
      Message.makeInquiry (Just question) foundAnswerWords foundAnswerQuestions
    (Nothing,  allAnswers) -> 
      Message.makeReference questionAnswer questionWords (answer : questionAnswers)


add (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) (Reference question@(Question Nothing questionWords [])) =
  case (Questions.find questionWords answerQuestions) of
    (Just (Question foundQuestionAnswer foundQuestionWords foundQuestionAnswers), otherQuestions) -> 
      Message.makeReference (Just answer) foundQuestionWords foundQuestionAnswers
    (Nothing, allQuestions) -> 
      Message.makeInquiry answerQuestion answerWords (question : answerQuestions)

add reference@(Reference _) addedReference@(Reference _) = reference
add inquiry@(Inquiry _) addedInquiry@(Inquiry _) = inquiry
add message Silence = message 
add Silence message = message
add _ _ = Silence