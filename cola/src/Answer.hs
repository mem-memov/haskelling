module Answer (hasWords, show) where

import MessageTypes
import Prelude hiding (show)

hasWords :: Answer -> Words -> Bool
hasWords (Answer _ answerWords _) words = words == answerWords

show :: Answer -> String
show (Answer _ answerWords _) = (getWords answerWords)
