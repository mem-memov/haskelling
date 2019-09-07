module Answer (hasWords, show) where

import MessageTypes
import Prelude hiding (show)

hasWords :: Words -> Answer -> Bool
hasWords words (Answer _ answerWords _) = words == answerWords

show :: Answer -> String
show (Answer _ answerWords _) = (getWords answerWords)
