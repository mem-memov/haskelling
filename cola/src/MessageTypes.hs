module MessageTypes(
Words(..),
Question(..),
Answer(..),
Message(..)
) where

newtype Words = Words { getWords :: String } deriving (Eq, Show, Read)
data Question = Question (Maybe Answer) Words [Answer] deriving (Show, Read)
data Answer = Answer (Maybe Question) Words [Question] deriving (Show, Read)
data Message = Silence | Reference Question | Inquiry Answer deriving (Show, Read)