module Answer (
  answerHasWords,
  showAnswer
) where

import MessageTypes

answerHasWords :: Answer -> Words -> Bool
answerHasWords (Answer _ answerWords _) words = words == answerWords

showAnswer :: Answer -> String
showAnswer (Answer _ answerWords _) = (getWords answerWords)
