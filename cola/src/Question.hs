module Question (hasWords, show) where

import AnswerQuestionType
import WordsType
import Prelude hiding (show)

hasWords :: Words -> Question -> Bool
hasWords words (Question _ questionWords _) = words == questionWords

show :: Question -> String
show (Question _ questionWords _) = "?" ++ (getWords questionWords)
