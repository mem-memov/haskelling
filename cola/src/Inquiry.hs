module Inquiry (fromAnswer, fromQuestion, fromReference) where

import MessageTypes

fromAnswer :: Answer -> Question -> Message
fromAnswer (Answer answerQuestion answerWords answerQuestions) question =
  Inquiry (Answer answerQuestion answerWords (question : answerQuestions))

fromQuestion :: Question -> Pick Answer -> Message
fromQuestion 
  (Question questionAnswer questionWords questionAnswers)
  (Pick (Just (Answer _ answerWords answerQuestions)) answers) = 
    Inquiry (Answer (Just (Question questionAnswer questionWords answers)) answerWords answerQuestions)

fromReference :: Message -> Message
fromReference (Reference (Question (Just (Answer answerQuestion answerWords answerQuestions)) questionWords questionAnswers)) =
  Inquiry (Answer (Just (Question Nothing questionWords questionAnswers)) answerWords answerQuestions)
