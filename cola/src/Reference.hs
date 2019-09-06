module Reference (fromAnswer, fromQuestion) where

import MessageTypes

fromAnswer :: Answer -> Pick Question -> Message
fromAnswer 
  (Answer answerQuestion answerWords answerQuestions)
  (Pick (Just (Question _ questionWords questionAnswers)) questions) = 
    Reference (Question (Just (Answer answerQuestion answerWords questions)) questionWords questionAnswers)

fromQuestion :: Question -> Answer -> Message
fromQuestion (Question questionAnswer questionWords questionAnswers) answer = 
  Reference (Question questionAnswer questionWords (answer : questionAnswers))