module Message (
  add,
  showFocus,
  comprehend,
  recall
) where

import MessageTypes
import qualified Answers (find)
import Questions
import Focus

makeInquiry :: Maybe Question -> Words -> [Question] -> Message
makeInquiry answerQuestion answerWords answerQuestions = Inquiry (Answer answerQuestion answerWords answerQuestions)

makeReference :: Maybe Answer -> Words -> [Answer] -> Message
makeReference questionAnswer questionWords questionAnswers = Reference (Question questionAnswer questionWords questionAnswers)

add :: Message -> Message -> Message

add (Reference question@(Question questionAnswer questionWords questionAnswers)) (Inquiry answer@(Answer Nothing answerWords [])) = 
  case (Answers.find answerWords questionAnswers) of
    (Just (Answer foundAnswerQuestion foundAnswerWords foundAnswerQuestions), otherAnswers) -> 
      makeInquiry (Just question) foundAnswerWords foundAnswerQuestions
    (Nothing,  allAnswers) -> 
      makeReference questionAnswer questionWords (answer : questionAnswers)


add (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) (Reference question@(Question Nothing questionWords [])) =
  case (findQuestion questionWords answerQuestions) of
    (Just (Question foundQuestionAnswer foundQuestionWords foundQuestionAnswers), otherQuestions) -> 
      makeReference (Just answer) foundQuestionWords foundQuestionAnswers
    (Nothing, allQuestions) -> 
      makeInquiry answerQuestion answerWords (question : answerQuestions)

add reference@(Reference _) addedReference@(Reference _) = reference
add inquiry@(Inquiry _) addedInquiry@(Inquiry _) = inquiry
add message Silence = message 
add Silence message = message
add _ _ = Silence



comprehend :: String -> Message
comprehend "" = Silence
comprehend ('?' : words) = makeReference Nothing (Words words) []
comprehend words = makeInquiry Nothing (Words words) []

recall :: String -> Message
recall contents
  | contents == "" = Silence
  | otherwise = read contents
