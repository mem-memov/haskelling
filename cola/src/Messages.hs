module Messages (add) where

import MessageTypes
import qualified Answers (find)
import qualified Questions (find)
import qualified Inquiry (fromAnswer, fromQuestion, hasSameWords, fromReference)
import qualified Reference (fromAnswer, fromQuestion, hasSameWords, fromInquiry)
import qualified Pick (hasContent)

add :: Message -> Message -> Message

add 
  (Reference question@(Question _ _ questionAnswers)) 
  (Inquiry answer@(Answer Nothing answerWords []))
  | Pick.hasContent pick = Inquiry.fromQuestion question pick
  | otherwise = Reference.fromQuestion question answer
  where pick = Answers.find answerWords questionAnswers

add 
  (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) 
  (Reference question@(Question Nothing questionWords [])) 
  | Pick.hasContent pick = Reference.fromAnswer answer pick
  | otherwise = Inquiry.fromAnswer answer question
  where pick = Questions.find questionWords answerQuestions

add 
  reference@(Reference _) 
  addedReference@(Reference _) 
  | Reference.hasSameWords reference addedReference = Inquiry.fromReference reference
  | otherwise = reference

add 
  inquiry@(Inquiry _) 
  addedInquiry@(Inquiry _)
  | Inquiry.hasSameWords inquiry addedInquiry = Reference.fromInquiry inquiry
  | otherwise = inquiry

add message Silence = message 
add Silence message = message
add _ _ = Silence