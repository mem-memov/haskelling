module Reference (fromWords, fromAnswer, fromQuestion, hasSameWords, fromInquiry) where

import MessageTypes

fromWords :: Words -> Message
fromWords words = Reference (Question Nothing words [])

fromAnswer :: Answer -> Pick Question -> Message
fromAnswer 
  (Answer answerQuestion answerWords answerQuestions)
  (Pick (Just (Question _ questionWords questionAnswers)) questions) = 
    Reference (Question (Just (Answer answerQuestion answerWords questions)) questionWords questionAnswers)

fromQuestion :: Question -> Answer -> Message
fromQuestion (Question questionAnswer questionWords questionAnswers) answer = 
  Reference (Question questionAnswer questionWords (answer : questionAnswers))

hasSameWords :: Message -> Message -> Bool
hasSameWords (Reference (Question _ questionWords _)) (Reference (Question _ otherQuestionWords _)) =
  questionWords == otherQuestionWords

fromInquiry :: Message -> Message
fromInquiry (Inquiry (Answer (Just (Question questionAnswer questionWords questionAnswers)) answerWords answerQuestions)) =
  Reference (Question questionAnswer questionWords ((Answer Nothing answerWords answerQuestions) : questionAnswers))
