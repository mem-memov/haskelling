module Inquiry (fromWords, fromAnswer, fromQuestion, hasSameWords, fromReference) where

import MessageTypes

fromWords :: Words -> Message
fromWords words = Inquiry (Answer Nothing words [])

fromAnswer :: Answer -> Question -> Message
fromAnswer (Answer answerQuestion answerWords answerQuestions) question =
  Inquiry (Answer answerQuestion answerWords (question : answerQuestions))

fromQuestion :: Question -> Pick Answer -> Message
fromQuestion 
  (Question questionAnswer questionWords questionAnswers)
  (Pick (Just (Answer _ answerWords answerQuestions)) answers) = 
    Inquiry (Answer (Just (Question questionAnswer questionWords answers)) answerWords answerQuestions)

hasSameWords :: Message -> Message -> Bool
hasSameWords (Inquiry (Answer _ answerWords _)) (Inquiry (Answer _ otherAnswerWords _)) =
  answerWords == otherAnswerWords

fromReference :: Message -> Message
fromReference (Reference (Question (Just (Answer answerQuestion answerWords answerQuestions)) questionWords questionAnswers)) =
  Inquiry (Answer answerQuestion answerWords ((Question Nothing questionWords questionAnswers) : answerQuestions))
  