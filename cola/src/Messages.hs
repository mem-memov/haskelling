module Messages (add) where

import MessageTypes
import qualified Answers (find)
import qualified Questions (find)
import qualified Inquiry (fromAnswer, fromQuestion)
import qualified Reference (fromAnswer, fromQuestion)
import qualified Pick (hasContent)

add :: Message -> Message -> Message

add (Reference question@(Question _ _ questionAnswers)) (Inquiry answer@(Answer Nothing answerWords [])) = 
  let pick = Answers.find answerWords questionAnswers in
    if Pick.hasContent pick 
      then Inquiry.fromQuestion question pick
      else Reference.fromQuestion question answer

add (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) (Reference question@(Question Nothing questionWords [])) =
  let pick = Questions.find questionWords answerQuestions in
    if Pick.hasContent pick 
      then Reference.fromAnswer answer pick
      else Inquiry.fromAnswer answer question

add reference@(Reference _) addedReference@(Reference _) = reference
add inquiry@(Inquiry _) addedInquiry@(Inquiry _) = inquiry
add message Silence = message 
add Silence message = message
add _ _ = Silence