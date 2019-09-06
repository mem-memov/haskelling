module Answer (
  answerHasWords
) where

import MessageTypes

answerHasWords :: Answer -> Words -> Bool
answerHasWords (Answer _ answerWords _) words = words == answerWords