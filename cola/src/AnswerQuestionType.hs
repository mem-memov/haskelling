module AnswerQuestionType (Answer(..), Question(..)) where

import WordsType

data Answer = Answer (Maybe Question) Words [Question] deriving (Eq, Show, Read)
data Question = Question (Maybe Answer) Words [Answer] deriving (Eq, Show, Read)